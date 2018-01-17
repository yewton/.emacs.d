;;; ytn-init-common.el --- System agnostic initialization -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Yuto SASAKI
;; Author: Yuto SASAKI <yewton@gmail.com>

;;; Commentary:

;; Initialize packages with `use-package'.

;;; Code:

(require 'dash)

(load-theme 'leuven t)

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

(f-mkdir ytn-var-directory)

(f-mkdir ytn-backup-directory)
(setq backup-directory-alist `((".*" . ,ytn-backup-directory)))

(require 'dired)
(setq package-enable-at-startup nil
      inhibit-startup-screen t
      initial-major-mode 'fundamental-mode
      save-abbrevs 'silently
      echo-keystrokes 0.2
      create-lockfiles nil
      eval-expression-print-length nil
      eval-expression-print-level nil
      disabled-command-function nil
      save-interprogram-paste-before-kill t
      delete-by-moving-to-trash t
      ring-bell-function 'ignore
      visible-bell nil
      dired-dwim-target t
      help-window-select t)

(add-hook 'prog-mode-hook #'goto-address-prog-mode)
(add-hook 'prog-mode-hook #'bug-reference-prog-mode)

(setq-default abbrev-mode t
              transient-mark-mode t
              sentence-end-double-space nil
              fill-column 120
              indent-tabs-mode nil
              tab-width 2
              require-final-newline t)

(setq abbrev-file-name (f-join ytn-var-directory "abbrev_defs")
      save-abbrevs t)
(setq-default abbrev-mode t)
(delight 'abbrev-mode "" 'abbrev)

(require 'whitespace)
(delight 'global-whitespace-mode "" 'whitespace)
(setq whitespace-style '(face tabs trailing spaces)
      ;; full-width space (\u3000, 　)
      whitespace-space-regexp "\\(\u3000+\\)")
(set-face-underline 'whitespace-space "pink")
(set-face-underline 'whitespace-trailing "pink")
(global-whitespace-mode)

(show-paren-mode)
(electric-pair-mode)

(require 'autorevert)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil
      auto-revert-mode-text nil)
(global-auto-revert-mode)

(require 'compile)
(setq compilation-scroll-output 'first-error)

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

(require 'server)
(defun server-remove-kill-buffer-hook ()
  "See https://stackoverflow.com/a/268205/2142831 ."
  (remove-hook 'kill-emacs-query-functions #'server-kill-emacs-query-function))
(add-hook 'server-visit-hook #'server-remove-kill-buffer-hook)
(unless (server-running-p) (server-start))

(eval-when-compile (require 'use-package))
(setq use-package-expand-minimally byte-compile-current-file)
(require 'delight)
(require 'bind-key)
(require 'f)
(require 'ytn-const)

(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(bind-key "C-c h" #'help-command)

(use-package ls-lisp
  :defer t
  :config
  (setq ls-lisp-dirs-first t
        ls-lisp-use-insert-directory-program nil
        ls-lisp-use-localized-time-format t))

(defun ytn-elisp-get-fnsymargs-string (oldfun sym &optional index prefix)
  "Apply OLDFUN SYM INDEX PREFIX."
  (let ((orig (apply oldfun sym index prefix)))
    (when orig
      (let* ((doc (s-replace-regexp "^.*advice:.*$"
                                    ""
                                    (or (ignore-errors (documentation sym)) "")))
             (doc (s-join "\n" (-take 3 (-filter #'s-present? (s-lines doc))))))
        (s-trim (s-concat orig "\n" doc))))))
(advice-add 'elisp-get-fnsym-args-string :around #'ytn-elisp-get-fnsymargs-string)

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
          try-complete-lisp-symbol))
  :bind* (("M-/" . hippie-expand)))

(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package nlinum
  :config
  (dolist (hook '(prog-mode-hook text-mode-hook conf-unix-mode-hook))
    (add-hook hook 'nlinum-mode)))

(use-package wdired
  :bind (:map dired-mode-map
              ("r" . wdired-change-to-wdired-mode)))




;; ------ vendor -------

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
         ("C-r" . ivy-previous-line)
         ("C-l" . ivy-backward-delete-char)))

(use-package swiper
  :config
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package counsel
  :config
  (setq counsel-describe-function-preselect 'ivy-function-called-at-point)
  (defun ytn-open-junk-file (&optional arg)
    "Open junk file using counsel.

  When ARG is non-nil search in junk files."
    (interactive "P")
    (let* ((open-junk-file-format (f-join ytn-var-directory "junk/%Y/%m/%d-%H%M%S."))
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
         ("<help> a" . counsel-apropos)
         ("<help> S" . counsel-info-lookup-symbol)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-rg)
         ("C-x l" . counsel-locate)
         ("C-x C-r" . counsel-recentf)
         ("M-y" . counsel-yank-pop)
         ("C-x C-z". ytn-open-junk-file)
         :map read-expression-map
         ("C-r" . counsel-minibuffer-history)))

(use-package ivy-xref
  :config
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package winum
  :demand t
  :delight
  :bind (:map winum-keymap
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

(use-package diff-hl
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'emacs-startup-hook 'global-diff-hl-mode))

(use-package flycheck
  :delight
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (add-hook 'emacs-startup-hook #'global-flycheck-mode))

(use-package flycheck-pos-tip
  :after flycheck
  :config
  (flycheck-pos-tip-mode))

(use-package projectile
  :delight
  :commands projectile-load-known-projects
  :demand t
  :config
  (let ((projectile-dir (f-join ytn-var-directory "projectile")))
    (f-mkdir projectile-dir)
    (setq projectile-enable-caching t
          projectile-ignored-projects '("/usr/local/")
          projectile-mode-line ""
          projectile-known-projects-file (f-join projectile-dir "projectile-bookmarks.eld")
          projectile-cache-file (f-join projectile-dir "projectile.cache")))
  (projectile-load-known-projects))

(use-package counsel-projectile
  :after projectile
  :delight
  :config
  (counsel-projectile-mode))

(use-package hydra
  :config
  (require 'ytn-hydra))

(use-package golden-ratio
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
  :delight
  :config
  (which-key-mode))

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package rainbow-identifiers
  :init
  (add-hook 'prog-mode-hook 'rainbow-identifiers-mode))

(use-package company
  :delight
  :init
  (add-hook 'emacs-startup-hook 'global-company-mode)
  :config
  (bind-key [remap next-line] 'company-select-next company-active-map)
  (bind-key [remap previous-line] 'company-select-previous company-active-map)
  (bind-key "C-f" 'company-show-location company-active-map)
  (bind-key "M-f" 'company-show-doc-buffer company-active-map)
  (bind-key [remap complete-symbol] 'counsel-company company-mode-map)
  (bind-key [remap completion-at-point] 'counsel-company company-mode-map))

(use-package powerline
  :config
  (setq powerline-height (+ (frame-char-height) 10)
        powerline-default-separator 'slant))

(use-package spaceline-config
  :config
  (require 'ytn-init-spaceline))

(use-package treemacs
  :defines (fa-fo)
  :config
  (setq treemacs-change-root-without-asking nil
        treemacs-collapse-dirs              (if (executable-find "python") 3 0)
        treemacs-file-event-delay           5000
        treemacs-follow-after-init          t
        treemacs-goto-tag-strategy          'refetch-index
        treemacs-indentation                2
        treemacs-indentation-string         " "
        treemacs-is-never-other-window      nil
        treemacs-never-persist              nil
        treemacs-no-png-images              nil
        treemacs-recenter-after-file-follow nil
        treemacs-recenter-after-tag-follow  nil
        treemacs-show-hidden-files          t
        treemacs-silent-filewatch           nil
        treemacs-silent-refresh             nil
        treemacs-sorting                    'alphabetic-desc
        treemacs-tag-follow-cleanup         t
        treemacs-tag-follow-delay           1.5
        treemacs-winum-number               10
        treemacs-width                      40
        treemacs-python-executable (executable-find "python3"))
  :bind (([f8] . treemacs-toggle)
         ("M-0" . treemacs-select-window)
         ("C-c 1" . treemacs-delete-other-windows)))

(use-package treemacs-filewatch-mode
  :after treemacs
  :commands treemacs-filewatch-mode
  :config
  (treemacs-filewatch-mode t))

(use-package treemacs-follow-mode
  :after treemacs
  :commands treemacs-follow-mode
  :config
  (treemacs-follow-mode t))


(use-package treemacs-async
  :after treemacs
  :commands treemacs-git-mode
  :config
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'extended))
    (`(t . _)
     (treemacs-git-mode 'simple))))

(use-package treemacs-projectile
  :config
  (setq treemacs-header-function 'treemacs-projectile-create-header)
  :bind (:map projectile-mode-map
              ([f8] . treemacs-projectile-toggle)
              ([f9] . treemacs-projectile)))

(use-package skk
  :config
  (require 'ytn-init-skk)
  :bind* (("C-x C-j" . skk-mode)))

(use-package rg
  :demand t
  :commands rg-enable-default-bindings
  :config
  (rg-enable-default-bindings (kbd "M-s"))
  (add-hook 'rg-mode-hook 'wgrep-ag-setup))

(use-package info+
  :after info
  :config
  ;; cf. https://www.emacswiki.org/emacs/info+.el
  (setq display-buffer-alist '(("*info*"
                                special-display-popup-frame
                                (background-color . "LightSteelBlue")
                                (height . 40)
                                (width . 80)
                                (unsplittable . t))))
  (set-face-attribute 'info-xref nil :underline nil))

(use-package help+)
(use-package help-fns+)

(use-package helpful
  :bind (("C-c h f" . helpful-callable)
         ("C-c h v" . helpful-variable)
         ("C-c h k" . helpful-key)
         ("C-c h C-d" . helpful-at-point)
         ("C-c h F" . helpful-function)
         ("C-c h C" . helpful-command)))

(provide 'ytn-init-common)
;;; ytn-init-common.el ends here
