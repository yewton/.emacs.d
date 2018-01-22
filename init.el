;; -*- lexical-binding: t; -*-

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(defvar generated-autoload-file)
(defvar make-backup-files)
(let* ((lisp-dir (expand-file-name "lisp" user-emacs-directory))
       (generated-autoload-file (expand-file-name "ytn-autoloads.el" lisp-dir)))
  (add-to-list 'load-path lisp-dir)

  (let ((make-backup-files nil))
    (update-directory-autoloads lisp-dir))
  (require 'ytn-autoloads)

  (ytn-init-bootstrap)
  (ytn-recipes-setup)
  (ytn-init-el-get)
  (ytn-init-install-packages)

  (byte-recompile-directory lisp-dir 0)

  (require 'ytn-init-common)

  (when (eq system-type 'darwin) (require 'ytn-init-system-darwin))
  (when (eq window-system 'ns) (require 'ytn-init-window-system-ns)))

(load custom-file 'noerror)

;; Local Variables:
;; no-byte-compile: t
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
