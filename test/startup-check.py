"""Run real startup and stale-bytecode checks in a disposable Emacs checkout."""

import argparse
import fcntl
import json
import os
from pathlib import Path
import pty
import select
import subprocess
import tempfile
import termios
import time


def run_startup(root, *, stale=False, timeout=180, gui=False):
    # Preserve checked-out source mtimes. Only disposable build products change.
    if stale:
        for directory in (root / "lisp", root / "lib"):
            for elc in directory.rglob("*.elc"):
                source = elc.with_suffix(".el")
                if source.exists() and not elc.is_symlink():
                    timestamp = source.stat().st_mtime - 1
                    os.utime(elc, (timestamp, timestamp))

    with tempfile.TemporaryDirectory(prefix="toncs-startup-") as directory:
        init_dir = Path(directory)
        report = init_dir / "report.txt"
        custom = init_dir / "custom.el"
        custom.touch()
        # Command-line --load runs after init.el. An early-init shim installs
        # monitoring before the real early-init.el and init.el are loaded.
        quote = lambda path: json.dumps(str(path))
        (init_dir / "early-init.el").write_text(
            ";;; -*- lexical-binding: t; -*-\n"
            f"(load {quote(root / 'test/startup-check.el')})\n"
            f"(load {quote(root / 'early-init.el')})\n"
            f"(setq custom-file {quote(custom)})\n"
        )
        (init_dir / "init.el").write_text(
            ";;; -*- lexical-binding: t; -*-\n"
            f"(load {quote(root / 'init.el')})\n"
        )
        env = dict(os.environ, TONCS_CHECK_REPORT=str(report),
                   TONCS_CHECK_ROOT=str(root), TONCS_CHECK_STATE=str(init_dir),
                   TERM="xterm-256color")
        command = ["emacs", f"--init-directory={init_dir}", "--no-site-file"]
        if not gui:
            command.append("--no-window-system")
        # A PTY permits an actual non-batch terminal startup on Linux and macOS.
        master, slave = pty.openpty()
        try:
            process = subprocess.Popen(
                command, stdin=slave, stdout=slave, stderr=slave, env=env,
                start_new_session=True,
                preexec_fn=lambda: fcntl.ioctl(slave, termios.TIOCSCTTY, 0),
            )
            output = bytearray()
            deadline = time.monotonic() + timeout
            while process.poll() is None and time.monotonic() < deadline:
                if select.select([master], [], [], 0.1)[0]:
                    output.extend(os.read(master, 65536))
            if process.poll() is None:
                process.kill()
                process.wait()
                print(f"Startup timed out after {timeout}s", flush=True)
                status = 1
            else:
                status = process.returncode
            if report.exists():
                print(report.read_text(), flush=True)
            else:
                print("Startup did not produce a completion report", flush=True)
                print(output.decode(errors="replace"), flush=True)
                status = 1
            return status
        finally:
            os.close(slave)
            os.close(master)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, required=True)
    parser.add_argument("--timeout", type=int, default=180)
    parser.add_argument("--gui", action="store_true", help="Use a graphical display")
    args = parser.parse_args()
    failed = False
    for stale in (False, True):
        print(f"Startup check: {'stale bytecode' if stale else 'built bytecode'}", flush=True)
        failed |= run_startup(args.root.resolve(), stale=stale,
                              timeout=args.timeout, gui=args.gui) != 0
    raise SystemExit(int(failed))
