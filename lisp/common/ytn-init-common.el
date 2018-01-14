;;; ytn-init-common.el --- System agnostic initialization -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Yuto SASAKI
;; Author: Yuto SASAKI <yewton@gmail.com>

;;; Commentary:

;; Initialize packages with `use-package'.

;;; Code:

(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)

(fset 'yes-or-no-p 'y-or-n-p)

(setq column-number-mode t
      window-combination-resize t)

(blink-cursor-mode 0)

(when window-system
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (scroll-bar-mode 0)
  (add-hook 'window-setup-hook #'toggle-frame-maximized))

(require 'ytn-const)
(f-mkdir ytn-backup-directory)
(setq backup-directory-alist `((".*" . ,ytn-backup-directory)))

(setq package-enable-at-startup nil
      inhibit-startup-screen t
      initial-major-mode 'fundamental-mode
      help-window-select t
      save-abbrevs 'silently
      echo-keystrokes 0.2
      create-lockfiles nil
      eval-expression-print-length nil
      eval-expression-print-level nil
      require-final-newline t
      disabled-command-function nil
      save-interprogram-paste-before-kill t
      delete-by-moving-to-trash t
      dired-dwim-target t
      ring-bell-function 'ignore
      visible-bell nil)

(add-hook 'prog-mode-hook #'goto-address-prog-mode)
(add-hook 'prog-mode-hook #'bug-reference-prog-mode)

(setq-default abbrev-mode t
              transient-mark-mode t
              sentence-end-double-space nil
              fill-column 120
              indent-tabs-mode nil
              tab-width 2)

(setq abbrev-file-name (f-join ytn-var-directory "abbrev_defs")
      save-abbrevs t)
(setq-default abbrev-mode t)

(show-paren-mode)
(electric-pair-mode)

(require 'autorevert)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(require 'server)
(defun server-remove-kill-buffer-hook ()
  (remove-hook 'kill-emacs-query-functions #'server-kill-emacs-query-function))
(add-hook 'server-visit-hook #'server-remove-kill-buffer-hook)
(unless (server-running-p) (server-start))

(require 'use-package)
(require 'delight)
(require 'bind-key)
(require 'f)
(require 'ytn-const)

(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(bind-key "C-c h" #'help-command)

(with-eval-after-load "compile"
  (setq compilation-scroll-output 'first-error))

(require 'recentf)
(setq recentf-save-file (f-join ytn-var-directory ".recentf")
      recentf-max-saved-items 1000
      recentf-exclude '(".recentf" "COMMIT_EDITMSG"))
(recentf-mode)
(run-with-idle-timer 300 t #'recentf-save-list)

(require 'time)
(setq display-time-format "%_m月%_d日(%a) %H:%M"
      display-time-default-load-average nil)
(display-time-mode)

(use-package ls-lisp
    :config
  (setq ls-lisp-dirs-first t
        ls-lisp-use-insert-directory-program nil
        ls-lisp-use-localized-time-format t))

(use-package hippie-exp
    :config
  (setq hippie-expand-try-functions-list
        '(try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))
(bind-key* "M-/" #'hippie-expand)

(require 'auto-compile)

(use-package auto-compile
    :demand t
    :config
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode))

;; cf. https://github.com/abo-abo/swiper/wiki/FAQ#sorting-commands-by-frequency
(use-package smex    
    :config
  (setq smex-save-file (f-join ytn-var-directory "smex-items")
        smex-history-length 9))

(use-package ivy
    :demand t
    :delight
    :config
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t
          enable-recursive-minibuffers t)
    :bind (("C-c C-r" . ivy-resume)
           ("<f6>" . ivy-resume)
           :map ivy-minibuffer-map
           ("C-l" . ivy-backward-delete-char)))

(use-package swiper
    :config
  ;; cf. http://rubikitch.com/2015/03/18/swiper/
  (defun isearch-forward-or-swiper (use-swiper)
    (interactive "P")
    (let (current-prefix-arg)
      (call-interactively (if use-swiper #'swiper #'isearch-forward))))
  :bind (("C-s" . isearch-forward-or-swiper)))

(use-package counsel
    :config
  (setq counsel-ag-base-command "rg --color never --no-heading %s")
  (defun my-open-junk-file (&optional arg)
    "Open junk file using counsel.

  When ARG is non-nil search in junk files."
    (interactive "P")
    (let* ((open-junk-file-format (expand-file-name "var/junk/%Y/%m/%d-%H%M%S." user-emacs-directory))
           (fname (format-time-string open-junk-file-format (current-time)))
           (rel-fname (file-name-nondirectory fname))
           (junk-dir (file-name-directory fname))
           (default-directory junk-dir))
      (if arg
          (counsel-ag nil junk-dir "" "[junk]")
        (f-mkdir junk-dir)
        (counsel-find-file rel-fname))))
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("<help> b" . counsel-descbinds)
         ("<help> f" . counsel-describe-function)
         ("<help> v" . counsel-describe-variable)
         ("<help> l" . counsel-load-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate)
         ("C-x C-r" . counsel-recentf)
         ("M-y" . counsel-yank-pop)
         ("C-x C-z". my-open-junk-file)
         :map read-expression-map
         ("C-r" . counsel-minibuffer-history)))

(use-package ivy-xref
    :demand t
    :config
    (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package winum
    :demand t
    :delight
    :bind (:map winum-keymap
                ("M-0" . 'winum-select-window-0-or-10)
                ("M-1" . 'winum-select-window-1)
                ("M-2" . 'winum-select-window-2)
                ("M-3" . 'winum-select-window-3)
                ("M-4" . 'winum-select-window-4)
                ("M-5" . 'winum-select-window-5)
                ("M-6" . 'winum-select-window-6)
                ("M-7" . 'winum-select-window-7)
                ("M-8" . 'winum-select-window-8)
                ("M-9" . 'winum-select-window-9))
    :config
    (winum-mode))

(use-package crux
    :init
  (global-set-key [remap move-beginning-of-line] 'crux-move-beginning-of-line)
  (global-set-key (kbd "C-c o") 'crux-open-with)
  (global-set-key [(shift return)] 'crux-smart-open-line)
  (global-set-key (kbd "s-r") 'crux-recentf-find-file)
  (global-set-key (kbd "C-<backspace>") 'crux-kill-line-backwards)
  (global-set-key [remap kill-whole-line] 'crux-kill-whole-line))

(use-package magit
    :defer t
    :config
    (setq magit-diff-refine-hunk 'all
          magit-git-executable (if (eq system-type 'windows-nt) "c:/Git/bin/git.exe" "git")))

(use-package magit-mode
    :defer t
    :config
    (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1))

(use-package elisp-mode
    :defer t
    :config
    (eval-when-compile (require 'cl-indent))
    (defun ytn-elisp-mode-hook ()
      (setq-local lisp-indent-function #'common-lisp-indent-function)
      (setq-local lisp-backquote-indentation nil))
    (add-hook 'emacs-lisp-mode-hook 'ytn-elisp-mode-hook))

(use-package flycheck
    :demand t
    :delight
    :config
    (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-pos-tip
    :after flycheck
    :config
    (flycheck-pos-tip-mode))

(eval-and-compile (require 'projectile))
(use-package projectile
    :demand t
    :delight
    :config
    (let ((projectile-dir (f-join ytn-var-directory "projectile")))
      (f-mkdir projectile-dir)
      (setq projectile-enable-caching t
            projectile-ignored-projects '("/usr/local/")
            projectile-mode-line ""
            projectile-known-projects-file (f-join projectile-dir "projectile-bookmarks.eld")
            projectile-cache-file (f-join projectile-dir "projectile.cache")))
    (projectile-mode)
    (projectile-load-known-projects))

(use-package counsel-projectile
    :after projectile
    :delight
    :config
    (counsel-projectile-mode))

(use-package hydra
    :demand t
    :config
    (require 'ytn-hydra))

(use-package golden-ratio
    :demand t
    :delight
    :config
    (require 'ytn-golden-ratio))

(use-package buffer-move
    :bind (("C-S-j" . buf-move-up)
           ("C-S-k" . buf-move-down)
           ("C-S-l" . buf-move-right)
           ("C-S-h" . buf-move-left)))

(use-package windmove
    :bind (("C-M-j" . windmove-up)
           ("C-M-k" . windmove-down)
           ("C-M-l" . windmove-right)
           ("C-M-h" . windmove-left)))

(use-package which-key
    :demand t
    :config
    (which-key-mode))

(provide 'ytn-init-common)
;;; ytn-init-common.el ends here
