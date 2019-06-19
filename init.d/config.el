;; -*- lexical-binding: t; -*-
(require 'f)
(require 'no-littering)

(eval-when-compile (require 'use-package))
(require 'bind-key)
(require 'delight)

(require 'ytn-init-autoloads)

(ytn-load-init-file "config-builtins")

;; cf. https://github.com/abo-abo/swiper/wiki/FAQ#sorting-commands-by-frequency
(use-package smex
  :config
  (setq smex-save-file (f-join no-littering-var-directory "smex-items")
        smex-history-length 9))

(use-package swiper
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package ivy
  :delight
  :demand t
  :commands (ivy-mode)
  :bind (("C-c C-r" . ivy-resume)
         ("<f6>" . ivy-resume)
         :map ivy-minibuffer-map
         ("C-r" . ivy-previous-line)
         ("C-l" . ivy-backward-delete-char))
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t
        ivy-height 30
        enable-recursive-minibuffers t))

(use-package ivy-hydra
  :after (ivy hydra))

(use-package counsel
  :commands (counsel-ag)
  :config
  (setq counsel-describe-function-preselect 'ivy-function-called-at-point
        counsel-find-file-ignore-regexp "\\(?:\\`[#.]\\)\\|\\(?:[#~]\\'\\)\\|\\(?:\\.elc\\'\\)")
  (defun ytn-open-junk-file (&optional arg)
    "Open junk file using counsel.

  When ARG is non-nil search in junk files."
    (interactive "P")
    (let* ((open-junk-file-format (f-join no-littering-var-directory "junk/%Y/%m/%d-%H%M%S."))
           (fname (format-time-string open-junk-file-format (current-time)))
           (rel-fname (file-name-nondirectory fname))
           (junk-dir (file-name-directory fname))
           (default-directory junk-dir))
      (if arg
          (counsel-ag nil junk-dir "" "[junk]")
        (mkdir junk-dir 'parents)
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
  :demand t
  :commands (ivy-xref-show-xrefs)
  :config
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package winum
  :commands (winum-mode winum-select-window-1 winum-select-window-2 winum-select-window-3 winum-select-window-4 winum-select-window-5
                        winum-select-window-6 winum-select-window-7 winum-select-window-8 winum-select-window-9)
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
  :init
  (winum-mode))

(use-package crux
  :init
  (bind-key [remap move-beginning-of-line] 'crux-move-beginning-of-line)
  (bind-key "C-c o" 'crux-open-with)
  (bind-key [(shift return)] 'crux-smart-open-line)
  (bind-key "C-<backspace>" 'crux-kill-line-backwards)
  (bind-key [remap kill-whole-line] 'crux-kill-whole-line))

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
  :commands (global-flycheck-mode)
  :init
  (add-hook 'emacs-startup-hook #'global-flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit))

(use-package flycheck-pos-tip
  :after flycheck
  :commands (flycheck-pos-tip-mode)
  :init
  (flycheck-pos-tip-mode))

(use-package projectile
  :delight
  :defines (projectile-mode-line)
  :commands (projectile-project-p projectile-load-known-projects)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (let ((projectile-dir (f-join no-littering-var-directory "projectile")))
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
  :commands (counsel-projectile-mode)
  :init
  (counsel-projectile-mode))

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
  :commands (which-key-mode)
  :init
  (which-key-mode))

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package rainbow-identifiers
  :init
  (add-hook 'prog-mode-hook 'rainbow-identifiers-mode))

(use-package company
  :delight
  :commands (global-company-mode)
  :init
  (add-hook 'emacs-startup-hook #'global-company-mode)
  :config
  (bind-key [remap next-line] 'company-select-next company-active-map)
  (bind-key [remap previous-line] 'company-select-previous company-active-map)
  (bind-key "C-f" 'company-show-location company-active-map)
  (bind-key "M-f" 'company-show-doc-buffer company-active-map)
  (bind-key [remap next-line] 'company-select-next company-search-map)
  (bind-key [remap previous-line] 'company-select-previous company-search-map)
  (bind-key [remap complete-symbol] 'counsel-company company-mode-map)
  (bind-key [remap completion-at-point] 'counsel-company company-mode-map))

(use-package company-quickhelp
  :after company
  :commands (company-quickhelp-mode)
  :config
  (setq company-quickhelp-use-propertized-text t)
  (company-quickhelp-mode))

(use-package powerline
  :config
  (setq powerline-height (+ (frame-char-height) 10)
        powerline-default-separator 'slant))

(use-package spaceline-config
  :config
  (require 'ytn-init-spaceline))

(use-package treemacs
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

  :bind (([f8] . ytn-treemacs)
         ("M-0" . treemacs-select-window)
         ("C-c 1" . treemacs-delete-other-windows)))

;;   (use-package treemacs-projectile
;;     :commands (treemacs-projectile)
;;     :config
;;     (use-package treemacs
;;       :commands (treemacs)
;;       :config
;;       (defun ytn-treemacs ()
;;         "Project context-aware treemacs.

;; If called in a project it calls `treemacs-projectile', otherwise `treemacs'."
;;         (interactive)
;;         (let ((fun (if (projectile-project-p) #'treemacs-projectile #'treemacs)))
;;           (call-interactively fun)))))

(use-package treemacs-filewatch-mode
  :after treemacs
  :commands (treemacs-filewatch-mode)
  :config
  (treemacs-filewatch-mode t))

(use-package treemacs-follow-mode
  :after treemacs
  :commands (treemacs-follow-mode)
  :config
  (treemacs-follow-mode t))

(use-package treemacs-async
  :after treemacs
  :commands (treemacs-git-mode)
  :config
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'extended))
    (`(t . _)
     (treemacs-git-mode 'simple))))

(use-package treemacs-projectile
  :config
  (setq treemacs-header-function 'treemacs-projectile-create-header))

(use-package grep
  :bind (:map grep-mode-map
              ("r" . wgrep-change-to-wgrep-mode)))

(use-package wgrep
  :defer t
  :config
  (setq wgrep-auto-save-buffer t)
  :bind (:map wgrep-mode-map
              ("C-c C-c" . wgrep-finish-edit)))

(use-package rg
  :commands (rg-enable-default-bindings)
  :bind (:map rg-mode-map
              ("r" . wgrep-change-to-wgrep-mode))
  :init
  (rg-enable-default-bindings (kbd "M-s")))

(use-package wgrep-ag
  :commands (wgrep-ag-setup)
  :after rg
  :init
  (add-hook 'rg-mode-hook #'wgrep-ag-setup))

(use-package info+
  :after info
  :config
  (set-face-attribute 'info-xref nil :underline nil))

(use-package ace-link
  :commands (ace-link-setup-default)
  :init
  (ace-link-setup-default))

(use-package help-mode
  :bind (:map help-mode-map
              ("j" . next-line)
              ("k" . previous-line)
              ("H" . help-go-back)
              ("h" . backward-char)
              ("L" . help-go-forward)
              ("l" . forward-char)
              ("v" . recenter-top-bottom)
              ("c" . counsel-ace-link)))

(use-package help+)
(use-package help-fns+)

(use-package helpful
  :bind (("<help> C-f" . helpful-callable)
         ("<help> C-v" . helpful-variable)
         ("<help> C-k" . helpful-key)
         ("<help> C-d" . helpful-at-point)
         ("<help> F" . helpful-function)
         ("<help> C" . helpful-command)))

(use-package emojify
  :bind (("C-c e" . emojify-insert-emoji))
  :commands (global-emojify-mode global-emojify-mode-line-mode)
  :init
  (add-hook 'emacs-startup-hook #'global-emojify-mode)
  (add-hook 'emacs-startup-hook #'global-emojify-mode-line-mode)
  :config
  (setq emojify-emoji-styles '(github unicode)))

(use-package browse-at-remote
  :defer t
  :config
  (setq browse-at-remote-prefer-symbolic nil))

(use-package yasnippet
  :delight yas-minor-mode
  :defer 5
  :commands (yas-global-mode)
  :init
  ;; yasnippet 読み込みと同時に `yasnippet-snippets-initialize' が走るから、先にセットしておかないといけない
  (setq yas-snippet-dirs `(,(expand-file-name "yasnippet/snippets" no-littering-etc-directory)))
  :config
  (yas-global-mode 1)
  (unbind-key "<tab>" yas-keymap))

(use-package diredfl
  :after dired
  :commands (diredfl-global-mode)
  :init
  (diredfl-global-mode))

(use-package ace-popup-menu
  :commands (ace-popup-menu-mode)
  :config
  (setq ace-popup-menu-show-pane-header t)
  (ace-popup-menu-mode))

(use-package leuven-theme
  :config
  (load-theme 'leuven t))

(use-package highlight-thing
  :delight
  :commands (global-highlight-thing-mode)
  :config
  (setq highlight-thing-exclude-thing-under-point t)
  (global-highlight-thing-mode))

(use-package hi-lock
  :delight)

(use-package visual-regexp
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "marked"
              markdown-fontify-code-blocks-natively t))

(use-package company-terraform
  :commands (company-terraform-init)
  :config
  (company-terraform-init))

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode)
  :config
  (setq scala-indent:use-javadoc-style t))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(use-package calendar
  :init
  (setq calendar-day-abbrev-array ["日" "月" "火" "水" "木" "金" "土"]
        calendar-day-header-array calendar-day-abbrev-array
        calendar-day-name-array (cl-map 'array (lambda (a) (format "%s曜日" a)) calendar-day-abbrev-array)
        calendar-month-name-array (cl-map 'array (lambda (a) (format "%2d月" a)) (number-sequence 1 12))
        calendar-month-abbrev-array calendar-month-name-array))

(use-package shackle
  :commands (shackle-mode)
  :init
  (setq shackle-rules '((compilation-mode :select nil)
                        (calendar-mode :select t :align 'below :size 0.45 :popup t))
        shackle-default-rule nil)
  (shackle-mode))

(use-package beacon
  :delight
  :commands (beacon-mode)
  :init
  ;; xref-pulse-momentarily の配色を参考に
  (setq beacon-color (face-attribute 'next-error :background))
  (beacon-mode 1))

(use-package org-cliplink
  :after (org)
  :commands (org-cliplink)
  :config
  ;; url-retrieve だと一部のHTTPS URLが空文字列で返ってくる。
  ;; 関係あり？ http://emacs.1067599.n8.nabble.com/bug-23225-25-1-50-url-retrieve-synchronously-having-trouble-with-some-https-URLs-td394451.html
  (setq org-cliplink-transport-implementation 'curl))

(ytn-load-init-file "config-skk")
(ytn-load-init-file "config-org")
(ytn-load-init-file "config-hydra")

(when (eq system-type 'darwin) (ytn-load-init-file "config-system-darwin"))
(when (eq window-system 'ns) (ytn-load-init-file "config-window-system-ns"))
(when (eq window-system 'x) (ytn-load-init-file "config-window-system-x"))
;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
