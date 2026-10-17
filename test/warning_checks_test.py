"""Regression tests for warning checks, using real Emacs subprocesses."""

import importlib.util
import contextlib
import io
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest
from unittest.mock import patch


HERE = Path(__file__).resolve().parent
SPEC = importlib.util.spec_from_file_location("startup_check", HERE / "startup-check.py")
STARTUP = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(STARTUP)


class WarningChecks(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory(prefix="toncs-warning-fixture-")
        self.addCleanup(self.directory.cleanup)
        self.root = Path(self.directory.name)
        (self.root / "test").mkdir()
        (self.root / "lisp").mkdir()
        for name in ("warning-monitor.el", "compile-check.el", "startup-check.el"):
            shutil.copyfile(HERE / name, self.root / "test" / name)

    def check_environment(self):
        # Fixture errors must not create real Actions annotations or artifacts.
        return {key: value for key, value in os.environ.items()
                if key not in ("GITHUB_ACTIONS", "GITHUB_STEP_SUMMARY", "TONCS_CHECK_OUTPUT")}

    def emacs(self, *args, env=None):
        return subprocess.run(["emacs", "-Q", "--batch", *args],
                              capture_output=True, text=True, timeout=30,
                              env=self.check_environment() | (env or {}))

    def startup(self, **kwargs):
        output = io.StringIO()
        with contextlib.redirect_stdout(output), patch.dict(
                os.environ, self.check_environment(), clear=True):
            status = STARTUP.run_startup(self.root, **kwargs)
        return status, output.getvalue()

    def compile(self, source, *, upstream=False, env=None):
        directory = self.root / "lib" / "fixture" if upstream else self.root / "lisp"
        directory.mkdir(parents=True, exist_ok=True)
        path = directory / "fixture.el"
        path.write_text(";;; fixture.el -*- lexical-binding: t; -*-\n" + source)
        return self.emacs(
            "--load", str(self.root / "test" / "warning-monitor.el"),
            "--eval", "(setq byte-compile-warnings '(not obsolete))",
            "--eval", f'(byte-compile-file "{path}")',
            "--funcall", "toncs-check-exit",
            env=env,
        )

    def test_clean_compilation_passes(self):
        result = self.compile("(defun fixture () 42)\n")
        self.assertEqual(result.returncode, 0, result.stderr)

    def test_obsolete_warning_survives_batch_suppression(self):
        result = self.compile(
            '(defun fixture-old () nil)\n'
            '(make-obsolete \'fixture-old "fixture-new" "1.0")\n'
            '(defun fixture () (fixture-old))\n'
        )
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("obsolete", result.stderr)

    def test_compiler_failure_is_not_lost(self):
        result = self.compile("(defun fixture () (let\n")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Compilation failed", result.stderr)

    def test_upstream_compiler_warning_passes_and_is_reported(self):
        summary = self.root / "summary.md"
        reports = self.root / "reports"
        result = self.compile("(defun fixture () unknown-variable)\n", upstream=True,
                              env={"GITHUB_ACTIONS": "true", "GITHUB_STEP_SUMMARY": str(summary),
                                   "TONCS_CHECK_OUTPUT": str(reports)})
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("::warning::[upstream] lib/fixture/fixture.el", result.stdout)
        self.assertIn("unknown-variable", summary.read_text())
        report = json.loads((reports / "check.json").read_text())
        self.assertEqual(report["failures"], 0)
        self.assertGreater(report["upstream_warnings"], 0)

    def test_upstream_compile_error_still_fails(self):
        result = self.compile("(defun fixture () (let\n", upstream=True)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Compilation failed", result.stderr)

    def runtime(self, library_body, expression, *, upstream=True):
        directory = self.root / "lib" / "fixture" if upstream else self.root / "lisp"
        directory.mkdir(parents=True, exist_ok=True)
        library = directory / "fixture.el"
        library.write_text(";;; -*- lexical-binding: t; -*-\n" + library_body)
        return self.emacs("--load", str(self.root / "test" / "warning-monitor.el"),
                          "--load", str(library), "--eval", expression,
                          "--funcall", "toncs-check-exit")

    def test_upstream_runtime_function_warning_passes(self):
        result = self.runtime("(defun fixture-warn () (display-warning 'fixture \"upstream warning\"))\n",
                              "(fixture-warn)")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("warning upstream", result.stderr)

    def test_upstream_runtime_error_fails(self):
        result = self.runtime("(defun fixture-warn () (display-warning 'fixture \"upstream error\" :error))\n",
                              "(fixture-warn)")
        self.assertNotEqual(result.returncode, 0)

    def test_configuration_runtime_warning_fails(self):
        result = self.runtime("(defun fixture-warn () (display-warning 'fixture \"configuration warning\"))\n",
                              "(fixture-warn)", upstream=False)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("warning configuration", result.stderr)

    def test_unknown_runtime_warning_fails(self):
        result = self.emacs("--load", str(self.root / "test" / "warning-monitor.el"),
                            "--eval", "(display-warning 'fixture \"unknown warning\")",
                            "--funcall", "toncs-check-exit")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("warning unknown", result.stderr)

    def test_top_level_warning_uses_loaded_source(self):
        result = self.runtime("(display-warning 'fixture \"top-level warning\")\n", "nil")
        self.assertEqual(result.returncode, 0, result.stderr)

    def test_unknown_source_is_not_inferred_from_warning_type(self):
        result = self.emacs("--load", str(self.root / "test" / "warning-monitor.el"),
                            "--eval", "(display-warning 'org-mem \"unknown source\")",
                            "--funcall", "toncs-check-exit")
        self.assertNotEqual(result.returncode, 0)

    def test_configuration_callback_from_upstream_is_fatal(self):
        callback = self.root / "lisp" / "callback.el"
        callback.write_text(
            ";;; -*- lexical-binding: t; -*-\n"
            "(defun fixture-callback () (display-warning 'fixture \"own callback\"))\n"
        )
        library = self.root / "lib" / "fixture" / "fixture.el"
        library.parent.mkdir(parents=True)
        library.write_text(";;; -*- lexical-binding: t; -*-\n(fixture-callback)\n")
        result = self.emacs("--load", str(self.root / "test" / "warning-monitor.el"),
                            "--load", str(callback), "--load", str(library),
                            "--funcall", "toncs-check-exit")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("warning configuration lisp/callback.el", result.stderr)

    def test_annotation_escapes_message_newlines(self):
        directory = self.root / "lib" / "fixture"
        directory.mkdir(parents=True)
        library = directory / "fixture.el"
        library.write_text(
            ";;; -*- lexical-binding: t; -*-\n"
            '(display-warning \'fixture "first%\\n::error::second")\n'
        )
        result = self.emacs("--load", str(self.root / "test" / "warning-monitor.el"),
                            "--load", str(library), "--funcall", "toncs-check-exit",
                            env={"GITHUB_ACTIONS": "true"})
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("first%25%0A::error::second", result.stdout)
        self.assertNotIn("\n::error::second", result.stdout)

    def test_forced_pass_ignores_newer_bytecode(self):
        source = self.root / "lisp" / "fixture.el"
        source.write_text(";;; -*- lexical-binding: t; -*-\n(defun fixture () 42)\n")
        result = self.emacs("--eval", f'(byte-compile-file "{source}")')
        self.assertEqual(result.returncode, 0, result.stderr)
        source.write_text(";;; -*- lexical-binding: t; -*-\n(defun fixture () unknown-variable)\n")
        bytecode = source.with_suffix(".elc")
        timestamp = bytecode.stat().st_mtime - 10
        os.utime(source, (timestamp, timestamp))
        borg = self.root / "lib" / "borg"
        borg.mkdir(parents=True)
        (borg / "borg.el").write_text("(defun borg-initialize () nil)\n(provide 'borg)\n")
        (self.root / "toncs-bootstrap.el").write_text(
            "(setq byte-compile-warnings '(not obsolete))\n"
        )
        result = self.emacs("--load", str(self.root / "test" / "compile-check.el"), "lisp")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("unknown-variable", result.stderr)

    def drone_fixture(self, broken=False):
        borg = self.root / "lib" / "borg"
        borg.mkdir(parents=True)
        (self.root / "toncs-bootstrap.el").write_text(";;; -*- lexical-binding: t; -*-\n")
        (borg / "borg.el").write_text(
            ";;; -*- lexical-binding: t; -*-\n"
            "(defun borg-initialize () nil)\n"
            "(defvar borg-minimal-emacs-alist nil)\n"
            "(defvar borg-compile-function nil)\n"
            "(defvar fixture-compiled-in-process nil)\n"
            "(defun borg--drone-disabled-p (_) nil)\n"
            "(defmacro borg-do-drones (spec &rest body) "
            "`(dolist (,(car spec) '(\"first\" \"second\")) ,@body))\n"
            "(defun borg-compile (drone) "
            '(when fixture-compiled-in-process (error "Shared compiler process")) '
            "(setq fixture-compiled-in-process t) "
            "(funcall borg-compile-function (expand-file-name "
            ' (concat "lib/" drone "/fixture.el"))))\n'
            "(provide 'borg)\n"
        )
        for name in ("first", "second"):
            directory = self.root / "lib" / name
            directory.mkdir()
            body = ("(defun fixture () (let\n" if broken and name == "second" else
                    "(defun fixture () unknown-variable)\n")
            (directory / "fixture.el").write_text(
                ";;; -*- lexical-binding: t; -*-\n" + body
            )
        return self.emacs("--load", str(self.root / "test" / "compile-check.el"), "drones",
                          env={"TONCS_CHECK_OUTPUT": str(self.root / "reports")})

    def test_drone_workers_are_separate_processes(self):
        result = self.drone_fixture()
        self.assertEqual(result.returncode, 0, result.stderr + result.stdout)
        for name in ("first", "second"):
            report = json.loads((self.root / "reports" / f"compile-drone-{name}.json").read_text())
            self.assertEqual(report["failures"], 0)
            self.assertGreater(report["upstream_warnings"], 0)

    def test_drone_worker_failure_propagates(self):
        result = self.drone_fixture(broken=True)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Drone check failed: second", result.stderr)
        report = json.loads((self.root / "reports" / "compile-drone-first.json").read_text())
        self.assertEqual(report["failures"], 0)
        self.assertIn("Compilation failed", result.stdout)

    def startup_fixture(self, body):
        (self.root / "early-init.el").write_text(
            ";;; -*- lexical-binding: t; -*-\n"
            "(setq user-emacs-directory (file-name-directory load-file-name))\n"
            '(setq user-init-file (expand-file-name "init.el" user-emacs-directory))\n'
        )
        (self.root / "init.el").write_text(
            ";;; -*- lexical-binding: t; -*-\n"
            "(when noninteractive (error \"Expected normal startup\"))\n"
            "(defvar toncs-config-prepare-functions '(toncs-config-fixture-prepare))\n"
            "(provide 'fixture)\n" + body
        )

    def test_normal_startup_runs_startup_hooks(self):
        self.startup_fixture(
            "(add-hook 'emacs-startup-hook "
            "(lambda () (display-warning 'fixture \"startup hook warning\")))\n"
        )
        status, output = self.startup(timeout=15)
        self.assertNotEqual(status, 0)
        self.assertIn("startup hook warning", output)

    def test_clean_normal_startup_passes(self):
        self.startup_fixture("")
        status, output = self.startup(timeout=15)
        self.assertEqual(status, 0, output)

    def test_upstream_warning_during_initialization_passes(self):
        library = self.root / "lib" / "fixture" / "fixture.el"
        library.parent.mkdir(parents=True)
        library.write_text(
            ";;; -*- lexical-binding: t; -*-\n"
            '(display-warning \'fixture "upstream initialization warning")\n'
        )
        self.startup_fixture(f"(load {json.dumps(str(library))})\n")
        status, output = self.startup(timeout=15)
        self.assertEqual(status, 0, output)
        self.assertIn("upstream initialization warning", output)

    def test_delayed_compiler_warnings_retain_upstream_source(self):
        library = self.root / "lib" / "fixture" / "fixture.el"
        library.parent.mkdir(parents=True)
        library.write_text(
            ";;; -*- lexical-binding: t; -*-\n"
            "(defun fixture-one () unknown-variable)\n"
            "(defun fixture-two () unknown-variable)\n"
        )
        self.startup_fixture(f"(byte-compile-file {json.dumps(str(library))})\n")
        status, output = self.startup(timeout=15)
        self.assertEqual(status, 0, output)
        self.assertIn("unknown-variable", output)

    def test_incomplete_startup_fails(self):
        self.startup_fixture("(error \"init failed\")\n")
        status, output = self.startup(timeout=5)
        self.assertNotEqual(status, 0)
        self.assertTrue("init-file error" in output or "timed out" in output, output)

    def test_startup_timeout_fails(self):
        self.startup_fixture("(add-hook 'emacs-startup-hook (lambda () (sleep-for 10)))\n")
        status, output = self.startup(timeout=0.5)
        self.assertNotEqual(status, 0)
        self.assertIn("timed out", output)

    def test_stale_bytecode_exercises_auto_compile(self):
        source = self.root / "lisp" / "fixture.el"
        source.write_text(";;; -*- lexical-binding: t; -*-\n(provide 'fixture)\n")
        result = self.emacs("--eval", f'(byte-compile-file "{source}")')
        self.assertEqual(result.returncode, 0, result.stderr)
        source.write_text(
            ";;; -*- lexical-binding: t; -*-\n"
            "(defun fixture-function () unknown-variable)\n(provide 'fixture)\n"
        )
        stamp = source.with_suffix(".elc").stat().st_mtime - 10
        os.utime(source, (stamp, stamp))
        paths = "".join(
            f'(add-to-list \'load-path "{HERE.parent / "lib" / package}")\n'
            for package in ("compat", "packed", "auto-compile")
        )
        self.startup_fixture(
            paths + "(setq features (delq 'fixture features))\n"
            f'(add-to-list \'load-path "{self.root / "lisp"}")\n'
            "(require 'auto-compile)\n(auto-compile-on-load-mode)\n"
        )
        status, output = self.startup(timeout=15)
        self.assertEqual(status, 0, output)
        status, output = self.startup(stale=True, timeout=15)
        self.assertNotEqual(status, 0)
        self.assertIn("unknown-variable", output)

    def test_magit_forge_load_stale_cyclic_dependencies(self):
        # Tangle the production configuration in the fixture, then supply
        # small cyclic packages with stale bytecode to exercise its loader.
        shutil.copyfile(HERE.parent / "setup.org", self.root / "setup.org")
        source = self.root / "lisp" / "toncs-config-magit.org"
        shutil.copyfile(HERE.parent / "lisp" / source.name, source)
        quote = lambda path: json.dumps(str(path))
        result = self.emacs("--load", "ob-tangle", "--eval",
                            f"(org-babel-tangle-file {quote(source)})")
        self.assertEqual(result.returncode, 0, result.stderr)
        for name in ("magit", "forge"):
            leaf = self.root / "lisp" / f"{name}-leaf.el"
            leaf.write_text(
                ";;; -*- lexical-binding: t; -*-\n"
                f"(defun fixture-{name}-value () 1)\n(provide '{name}-leaf)\n"
            )
            result = self.emacs("--eval", f"(byte-compile-file {quote(leaf)})")
            self.assertEqual(result.returncode, 0, result.stderr)
            leaf.write_text(
                ";;; -*- lexical-binding: t; -*-\n"
                f"(require '{name})\n"
                f"(defun fixture-{name}-value () 2)\n(provide '{name}-leaf)\n"
            )
            stamp = leaf.stat().st_mtime - 1
            os.utime(leaf.with_suffix(".elc"), (stamp, stamp))
            (self.root / "lisp" / f"{name}.el").write_text(
                ";;; -*- lexical-binding: t; -*-\n"
                f"(provide '{name})\n(require '{name}-leaf)\n"
            )
        paths = [HERE.parent / "lib" / package
                 for package in ("compat", "packed", "auto-compile")]
        paths.append(self.root / "lisp")
        script = "".join(f"(add-to-list 'load-path {quote(path)})\n" for path in paths)
        script += (
            "(setq load-prefer-newer t)\n"
            "(require 'auto-compile)\n(auto-compile-on-load-mode 1)\n"
            "(defvar fixture-compilations nil)\n"
            "(advice-add 'byte-compile-file :before "
            "(lambda (file &rest _) (push file fixture-compilations)))\n"
            "(require 'toncs-config-magit)\n"
            "(when fixture-compilations "
            "(error \"Nested compilation: %S\" fixture-compilations))\n"
            "(unless (and (= (fixture-magit-value) 2) (= (fixture-forge-value) 2)) "
            "(error \"Updated dependency sources were not loaded\"))\n"
        )
        path = self.root / "check.el"
        path.write_text(";;; -*- lexical-binding: t; -*-\n" + script)
        result = self.emacs("--load", str(path))
        self.assertEqual(result.returncode, 0, result.stderr)


if __name__ == "__main__":
    unittest.main()
