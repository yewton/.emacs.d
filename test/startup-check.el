;;; startup-check.el --- Instrument a normal Emacs startup -*- lexical-binding: t; -*-

(load (expand-file-name "warning-monitor.el" (file-name-directory load-file-name)))
(require 'subr-x)

;; This check covers synchronous byte compilation.  Native compilation needs
;; its own worker completion and diagnostic collection protocol.
(setq native-comp-jit-compilation nil)
(setq auto-compile-native-compile nil)
(setq server-name (format "toncs-check-%s" (emacs-pid)))
(setq server-socket-dir (expand-file-name "server/" (getenv "TONCS_CHECK_STATE")))

(defun toncs-startup-check-finish ()
  "Exercise configured features, save diagnostics, and exit."
  (condition-case err
      (progn
        (when noninteractive
          (error "Expected a non-batch startup"))
        (unless (file-equal-p user-emacs-directory (getenv "TONCS_CHECK_ROOT"))
          (error "Unexpected user-emacs-directory: %s" user-emacs-directory))
        (when init-file-had-error
          (error "Emacs reported an init-file error"))
        (unless (bound-and-true-p toncs-config-prepare-functions)
          (error "Configuration feature registry was not initialized"))
        (dolist (prepare-function toncs-config-prepare-functions)
          (let ((feature (intern
                          (string-remove-suffix
                           "-prepare"
                           (string-remove-prefix "toncs-config-"
                                                 (symbol-name prepare-function))))))
            (condition-case err
                (require feature)
              (error
               (toncs-check-record "Loading %s: %s" feature
                                   (error-message-string err)))))))
    (error (toncs-check-record "Startup: %s" (error-message-string err))))
  (with-temp-file (getenv "TONCS_CHECK_REPORT")
    (insert (format "Warning check: %d diagnostic(s)\n"
                    (length toncs-check-diagnostics)))
    (insert (toncs-check-diagnostic-text) "\n"
            (toncs-check-annotation-text) "\n")
    (dolist (name '("*Compile-Log*" "*Warnings*" "*Messages*"))
      (when (get-buffer name)
        (insert "\n" name "\n" (with-current-buffer name (buffer-string))))))
  (toncs-check-exit))

(add-hook 'emacs-startup-hook
          (lambda () (run-at-time 1 nil #'toncs-startup-check-finish)) t)
;;; startup-check.el ends here
