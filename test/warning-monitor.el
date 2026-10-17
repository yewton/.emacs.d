;;; warning-monitor.el --- Collect diagnostics for CI -*- lexical-binding: t; -*-

(require 'bytecomp)
(require 'cl-lib)

(defvar toncs-check-diagnostics nil
  "Warnings and compilation failures observed by the current check.")

(defun toncs-check-record (format-string &rest args)
  "Record and print a diagnostic described by FORMAT-STRING and ARGS."
  (let ((text (apply #'format format-string args)))
    ;; Emacs may replay delayed warnings after startup.
    (unless (member text toncs-check-diagnostics)
      (push text toncs-check-diagnostics)
      (message "CHECK: %s" text))))

(defun toncs-check-byte-warning (text &optional _fill level)
  "Record compiler TEXT at LEVEL, including errors reported as diagnostics."
  (toncs-check-record "byte compiler %s (%s): %s"
                      (or byte-compile-current-file "internal form")
                      (or level 'warning) text))

(defun toncs-check-compile (function file &rest args)
  "Call compiler FUNCTION for FILE with ARGS and record failures.
Enable every compiler warning even if bootstrap or a package disabled them."
  (let ((byte-compile-warnings t)
        ;; Collect all warnings; report failure after completing the check.
        (byte-compile-error-on-warn nil))
    (condition-case err
        (let ((result (apply function file args)))
          (unless result
            (toncs-check-record "Compilation failed: %s" file))
          result)
      (error
       (toncs-check-record "Compilation raised an error in %s: %s"
                           file (error-message-string err))
       (signal (car err) (cdr err))))))

(defun toncs-check-warning (type text &optional level &rest _args)
  "Record runtime warnings of TYPE with TEXT and LEVEL before filtering."
  (when (and (not (eq (if (consp type) (car type) type) 'bytecomp))
             (memq (or level :warning) '(:warning :error :emergency)))
    (toncs-check-record "runtime %S (%s): %s" type (or level :warning) text)))

(advice-add 'byte-compile-log-warning :before #'toncs-check-byte-warning)
(advice-add 'byte-compile-file :around #'toncs-check-compile)
(advice-add 'display-warning :before #'toncs-check-warning)

(defun toncs-check-exit ()
  "Exit unsuccessfully if any diagnostics were collected."
  (message "Warning check: %d diagnostic(s)" (length toncs-check-diagnostics))
  (kill-emacs (if toncs-check-diagnostics 1 0)))

(provide 'warning-monitor)
;;; warning-monitor.el ends here
