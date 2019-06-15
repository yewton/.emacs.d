;; -*- lexical-binding: t; -*-

;; cf. https://www.reddit.com/r/emacs/comments/56fvgd/is_there_a_way_to_stop_emacs_from_adding_the/
(setq package--init-file-ensured t)

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(defvar ytn-lisp-dir (expand-file-name "lisp" user-emacs-directory))
(defvar ytn-lisp-dir (expand-file-name "lisp" user-emacs-directory))

(require 'files) ; for make-backup-files
(require 'autoload)

(defun ytn-update-autoloads ()
  (interactive)
  (let ((generated-autoload-file (expand-file-name "ytn-init-autoloads.el" ytn-lisp-dir))
        (make-backup-files nil))
    (update-directory-autoloads ytn-lisp-dir)))

(add-to-list 'load-path ytn-lisp-dir)

(ytn-update-autoloads)
(require 'ytn-init-autoloads)

(ytn-init-install-el-get)
(ytn-init-set-el-get-sources)
(ytn-init-install-build-essentials)
(ytn-init-install-packages)

(byte-recompile-directory ytn-lisp-dir 0)

(ytn-init-configure)

(load custom-file 'noerror)

;; Local Variables:
;; no-byte-compile: t
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
