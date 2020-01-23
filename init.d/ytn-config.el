;; -*- lexical-binding: t; -*-
(require 'f)
(require 'dash)
(require 'no-littering)

(eval-when-compile (require 'use-package))
(require 'bind-key)
(require 'delight)

(load "ytn-config-builtins")

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
  :hook ((magit-post-refresh . diff-hl-magit-post-refresh)
         (prog-mode . turn-on-diff-hl-mode)
         (vc-dir-mode-hook . turn-on-diff-hl-mode)))

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
  :commands (projectile-mode projectile-project-p projectile-load-known-projects)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (let ((projectile-dir (f-join no-littering-var-directory "projectile")))
    (f-mkdir projectile-dir)
    (setq projectile-enable-caching t
          projectile-ignored-projects '("/usr/local/")
          projectile-known-projects-file (f-join projectile-dir "projectile-bookmarks.eld")
          projectile-cache-file (f-join projectile-dir "projectile.cache")))
  (projectile-load-known-projects)
  :init
  (projectile-mode +1))

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
  :init
  (setq markdown-command "commonmarker --extension=tagfilter,autolink,table,strikethrough"
        markdown-fontify-code-blocks-natively t
        markdown-asymmetric-header t
        markdown-css-paths '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"
                             "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css")
        markdown-xhtml-header-content "<script src=\"https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js\"></script><script>hljs.initHighlightingOnLoad();</script>"
        markdown-xhtml-body-preamble "<div class=\"markdown-body\">"
        markdown-xhtml-body-epilogue "</div>"))

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

(use-package migemo
  :demand t
  :commands migemo-init
  :config
  (let* ((dict-candidates (list "/usr/local/Cellar/cmigemo/20110227/share/migemo/utf-8/migemo-dict"
                                "/usr/local/share/migemo/utf-8/migemo-dict"))
         (dict (--find (f-readable-p it) dict-candidates)))
    (when dict
      (setq migemo-dictionary dict)))
  (setq migemo-user-dictionary (f-join no-littering-var-directory "migemo-user-dict"))
  (setq migemo-regex-dictionary (f-join no-littering-var-directory "migemo-regex-dict"))
  (migemo-init))

(use-package japanese-holidays
  :after calendar holidays
  :config
  (setq calendar-holidays japanese-holidays holiday-local-holidays holiday-other-holidays)
  (setq japanese-holiday-weekend '(0 6)     ; 土日を祝日として表示
        japanese-holiday-weekend-marker     ; 土曜日を水色で表示
        '(holiday nil nil nil nil nil japanese-holiday-saturday))
  (add-hook 'calendar-today-visible-hook 'japanese-holiday-mark-weekend)
  (add-hook 'calendar-today-invisible-hook 'japanese-holiday-mark-weekend)
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today))

(load "ytn-config-skk")
(load "ytn-config-org")
(load "ytn-config-ivy")
(load "ytn-config-hydra")
(load "ytn-config-treemacs")
(load "ytn-config-spaceline")

(when (eq system-type 'darwin) (load "ytn-config-system-darwin"))
(when (eq window-system 'ns) (load "ytn-config-window-system-ns"))
(when (eq window-system 'x) (load "ytn-config-window-system-x"))
;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
