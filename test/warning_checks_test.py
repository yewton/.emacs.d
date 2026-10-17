"""Regression tests for warning checks, using real Emacs subprocesses."""

import importlib.util
import contextlib
import io
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest


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

    def emacs(self, *args):
        return subprocess.run(["emacs", "-Q", "--batch", *args],
                              capture_output=True, text=True, timeout=30)

    def startup(self, **kwargs):
        output = io.StringIO()
        with contextlib.redirect_stdout(output):
            status = STARTUP.run_startup(self.root, **kwargs)
        return status, output.getvalue()

    def compile(self, source):
        path = self.root / "lisp" / "fixture.el"
        path.write_text(";;; fixture.el -*- lexical-binding: t; -*-\n" + source)
        return self.emacs(
            "--load", str(HERE / "warning-monitor.el"),
            "--eval", "(setq byte-compile-warnings '(not obsolete))",
            "--eval", f'(byte-compile-file "{path}")',
            "--funcall", "toncs-check-exit",
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

    def test_incomplete_startup_fails(self):
        self.startup_fixture("(error \"init failed\")\n")
        status, output = self.startup(timeout=5)
        self.assertNotEqual(status, 0)
        self.assertTrue("init-file error" in output or "timed out" in output, output)

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


if __name__ == "__main__":
    unittest.main()
