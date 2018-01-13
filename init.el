;; -*- lexical-binding: t; -*-

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(setq load-prefer-newer t
      package-enable-at-startup nil)

;; cf. https://emacs.stackexchange.com/a/35953/18118
(require 'gnutls)
(add-to-list 'gnutls-trustfiles "/private/etc/ssl/cert.pem")

(add-to-list 'load-path (expand-file-name "el-get/el-get" user-emacs-directory))

(setq el-get-install-skip-emacswiki-recipes t
      el-get-install-shallow-clone t)
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/yewton/el-get/fa6ee7cd5e19952a3a636fb5bb0ad18491845db4/el-get-install.el")
    (let (el-get-install-skip-emacswiki-recipes
          el-get-install-shallow-clone)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(setq el-get-git-shallow-clone t
      el-get-verbose t)
(with-eval-after-load "el-get"
  ;; cf. https://github.com/dimitri/el-get/pull/2598
  (add-to-list 'el-get-git-known-smart-domains "code.orgmode.org"))
(add-to-list 'el-get-recipe-path (expand-file-name "el-get-user/recipes" user-emacs-directory))

(let* ((lisp-dir (expand-file-name "lisp" user-emacs-directory))
       (recipes-dir (expand-file-name "recipes" lisp-dir))
       (common-dir (expand-file-name "common" lisp-dir))
       (system-base-dir (expand-file-name "system" lisp-dir))
       (system-dir (expand-file-name (symbol-name system-type) system-base-dir))
       (window-system-base-dir (expand-file-name "window-system" lisp-dir))
       (window-system-dir (and window-system
                               (expand-file-name (symbol-name window-system) window-system-base-dir))))
  (dolist (dir (list recipes-dir common-dir system-dir window-system-dir))
    (when dir (add-to-list 'load-path dir)))

  (byte-recompile-directory recipes-dir)
  (require 'ytn-recipes)

  (el-get 'sync (mapcar #'el-get-source-name ytn-base-recipes))

  ;; to avoid build errors
  (el-get 'sync '(exec-path-from-shell use-package))
  (use-package exec-path-from-shell
    :demand t
    :config
    (when (memq window-system '(mac ns x))
      (setq exec-path-from-shell-arguments (list "-l"))
      (exec-path-from-shell-initialize)))

  (el-get 'sync (mapcar #'el-get-source-name ytn-recipes))

  (dolist (dir (list common-dir system-dir window-system-dir))
    (when (file-directory-p dir)
        (byte-recompile-directory dir 0)))

  (require 'ytn-init-common)
  (require 'ytn-init-system nil t)
  (require 'ytn-init-window-system nil t))

;; Local Variables:
;; no-byte-compile: t
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
