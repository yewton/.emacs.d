;;; compile-check.el --- Force compilation and check diagnostics -*- lexical-binding: t; -*-

(load (expand-file-name "warning-monitor.el" (file-name-directory load-file-name)))

(let* ((root (file-name-directory
              (directory-file-name (file-name-directory load-file-name))))
       (mode (pop command-line-args-left))
       (default-directory root))
  (setq toncs-check-label (concat "compile-" mode))
  (condition-case err
      (progn
        (load (expand-file-name "toncs-bootstrap.el" root))
        (add-to-list 'load-path (expand-file-name "lib/borg" root))
        (require 'borg)
        (borg-initialize)
        (pcase mode
          ("lisp"
           ;; Always compile, including when .elc files are newer than sources.
           (dolist (file (directory-files (expand-file-name "lisp" root)
                                         t "\\.el\\'"))
             (condition-case err
                 (byte-compile-file file)
               (error (toncs-check-record "%s" (error-message-string err))))))
          ("drone"
           (let ((drone (pop command-line-args-left))
                 (borg-compile-function #'byte-compile-file))
             (setq toncs-check-label (concat "compile-drone-" drone))
             ;; Honor Borg's configured paths and file exclusions. Bootstrap
             ;; has already produced each package's custom build products.
             (borg-compile drone)))
          ("drones"
           ;; Packages can install advice while compiling (e.g. DDskk).
           ;; A fresh compiler per package prevents that state from leaking
           ;; into another package's macro expansion or bytecode constants.
           (borg-do-drones (drone)
             (unless (or (borg--drone-disabled-p drone)
                         (let ((min (cdr (assoc drone borg-minimal-emacs-alist))))
                           (and min (version< emacs-version min))))
               (with-temp-buffer
                 (let ((status
                        (call-process (expand-file-name invocation-name invocation-directory)
                                      nil t nil "--quick" "--batch" "--load"
                                      (expand-file-name "test/compile-check.el" root)
                                      "drone" drone)))
                   (princ (buffer-string))
                   (unless (eq status 0)
                     (toncs-check-record "Drone check failed: %s (exit %s)" drone status)))))))
          (_ (error "Expected mode lisp, drones, or drone, got %S" mode))))
    (error (toncs-check-record "%s" (error-message-string err)))))

(toncs-check-exit)
;;; compile-check.el ends here
