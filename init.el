;; -*- lexical-binding: t; -*-

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(setq load-prefer-newer t)

;; cf. https://emacs.stackexchange.com/a/35953/18118
(require 'gnutls)
(add-to-list 'gnutls-trustfiles "/private/etc/ssl/cert.pem")

(add-to-list 'load-path (expand-file-name "el-get/el-get" user-emacs-directory))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/yewton/el-get/fa6ee7cd5e19952a3a636fb5bb0ad18491845db4/el-get-install.el")
    (let (el-get-install-skip-emacswiki-recipes
          el-get-install-shallow-clone)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(setq el-get-verbose t
      debug-on-error t
      package-user-dir (expand-file-name (convert-standard-filename "var/elpa") user-emacs-directory))
(with-eval-after-load "el-get"
  ;; cf. https://github.com/dimitri/el-get/pull/2598
  (add-to-list 'el-get-git-known-smart-domains "code.orgmode.org"))
(add-to-list 'el-get-recipe-path (expand-file-name (convert-standard-filename "etc/el-get-recipes/")
                                                   user-emacs-directory))

(let* ((lisp-dir (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path lisp-dir)

  (require 'ytn-recipes)

  (let ((el-get-use-autoloads nil))
    (el-get 'sync '(exec-path-from-shell no-littering)))

  ;; to avoid build errors due to PATH
  (when (memq window-system '(ns x))
    (require 'exec-path-from-shell)
    (setq exec-path-from-shell-arguments (list "-l"))
    (exec-path-from-shell-initialize))

  ;; to avoid littering
  (require 'no-littering)

  (el-get 'sync (mapcar #'el-get-source-name el-get-sources))

  (byte-recompile-directory lisp-dir 0)

  (require 'ytn-init-common)

  (when (eq system-type 'darwin) (require 'ytn-init-system-darwin))
  (when (eq window-system 'ns) (require 'ytn-init-window-system-ns)))

;; Local Variables:
;; no-byte-compile: t
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
