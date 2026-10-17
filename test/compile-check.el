;;; compile-check.el --- Force compilation and check diagnostics -*- lexical-binding: t; -*-

(load (expand-file-name "warning-monitor.el" (file-name-directory load-file-name)))

(let* ((root (file-name-directory
              (directory-file-name (file-name-directory load-file-name))))
       (mode (pop command-line-args-left))
       (default-directory root))
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
          ("drones"
           ;; Bootstrap has run each package's custom build steps already.
           ;; Use Borg's load paths and no-byte-compile exclusions for this
           ;; independent compilation pass, without cleaning build products.
           (let ((borg-compile-function #'byte-compile-file))
             (borg-do-drones (drone)
               (unless (or (borg--drone-disabled-p drone)
                           (let ((min (cdr (assoc drone borg-minimal-emacs-alist))))
                             (and min (version< emacs-version min))))
                 (condition-case err
                     (borg-compile drone)
                   (error
                    (toncs-check-record "%s: %s" drone
                                        (error-message-string err))))))))
          (_ (error "Expected mode lisp or drones, got %S" mode))))
    (error (toncs-check-record "%s" (error-message-string err)))))

(toncs-check-exit)
;;; compile-check.el ends here
