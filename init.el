;; -*- lexical-binding: t; -*-

;; cf. https://blog.d46.us/advanced-emacs-startup/#org37ca53c
(setq gc-cons-threshold (* 50 1000 1000))

;; cf. https://www.reddit.com/r/emacs/comments/56fvgd/is_there_a_way_to_stop_emacs_from_adding_the/
(setq package--init-file-ensured t)

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory)
      load-prefer-newer t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'ytn-lib)

(advice-add 'message :before #'ytn-ad-timestamp-message)

(require 'ytn-init-install-el-get)
(ytn-init-install-el-get)

(require 'ytn-init-set-el-get-sources)
(ytn-init-set-el-get-sources)

(require 'ytn-init-install-build-essentials)
(ytn-init-install-build-essentials)

(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

(require 'ytn-init-install-packages)
(ytn-init-install-packages)

(byte-recompile-directory ytn-lisp-dir 0)
(byte-recompile-directory ytn-init-dir 0)

(add-to-list 'load-path ytn-init-dir)

(add-to-list 'exec-path (expand-file-name "bin/" user-emacs-directory))
(load "ytn-config")

(load custom-file 'noerror)

(setq gc-cons-threshold (* 2 1000 1000))

;; Local Variables:
;; no-byte-compile: t
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
