;; -*- lexical-binding: t -*-
(require 'autorevert)
(require 'compile)
(require 'dired)
(require 'hl-line)
(require 'no-littering)
(require 'recentf)
(require 'server)
(require 'time)
(require 'uniquify)
(require 'whitespace)

(eval-when-compile (require 'use-package))

(require 'bind-key)
(require 'delight)
(require 'f)

(defun server-remove-kill-buffer-hook ()
  "See https://stackoverflow.com/a/268205/2142831 ."
  (remove-hook 'kill-emacs-query-functions #'server-kill-emacs-query-function))

(defun ytn-elisp-get-fnsym-args-string (oldfun sym &optional index prefix)
  "Apply OLDFUN SYM INDEX PREFIX."
  (let ((orig (apply oldfun sym index prefix)))
    (when orig
      (let* ((doc (s-replace-regexp "^.*advice:.*$"
                                    ""
                                    (or (ignore-errors (documentation sym)) "")))
             (doc (s-join "\n" (-take 3 (-filter #'s-present? (s-lines doc))))))
        (s-trim (s-concat orig "\n" doc))))))

(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)

(fset 'yes-or-no-p 'y-or-n-p)

(setq column-number-mode t
      window-combination-resize t)

(blink-cursor-mode 1)

(auto-save-visited-mode +1)
(save-place-mode t)

(menu-bar-mode 0)
(when window-system
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (add-hook 'window-setup-hook #'toggle-frame-maximized))

(setq scroll-conservatively 0
      scroll-preserve-screen-position t)
(setq-default scroll-up-aggressively 0.8
              scroll-down-aggressively 0.8)

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

(use-package js
  :defer t
  :config
  (setq js-indent-level 2))

(setq abbrev-file-name (f-join no-littering-var-directory "abbrev_defs")
      save-abbrevs t)
(setq-default abbrev-mode t)
(delight 'abbrev-mode "" 'abbrev)

(delight 'global-whitespace-mode "" 'whitespace)
(setq whitespace-style '(face tabs trailing spaces)
      ;; full-width space (\u3000, 　)
      whitespace-space-regexp "\\(\u3000+\\)")
(set-face-underline 'whitespace-space "pink")
(set-face-underline 'whitespace-trailing "pink")
(global-whitespace-mode)

(require 'paren)
(show-paren-mode)
(setq show-paren-style 'mixed)
(electric-pair-mode)

(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil
      auto-revert-mode-text nil)
(global-auto-revert-mode)

(setq compilation-scroll-output 'first-error)

(setq recentf-save-file (f-join no-littering-var-directory ".recentf")
      recentf-max-saved-items 1000
      recentf-exclude '(".recentf" "COMMIT_EDITMSG"))
(recentf-mode)
(run-with-idle-timer 300 t #'recentf-save-list)

(setq display-time-format "%_m月%_d日(%a) %H:%M"
      display-time-default-load-average nil)
(display-time-mode)

(add-hook 'server-visit-hook #'server-remove-kill-buffer-hook)
(unless (server-running-p) (server-start))

(dolist (hook '(prog-mode-hook text-mode-hook conf-unix-mode-hook))
  (add-hook hook 'display-line-numbers-mode)
  (add-hook hook 'hl-line-mode))

;; https://stackoverflow.com/a/17630877/2142831

(defface hl-line-agenda-face
  '((t :box (:color "deep pink" :line-width 1 :style nil) :extend t))
  "Face for highlighting the current line in org-agenda"
  :group 'hl-line)

;; The function to use the new face
(defun my-org-agenda-hl-line ()
  (set (make-local-variable 'hl-line-face) ; This is how to make it local
       'hl-line-agenda-face)
    (hl-line-mode))

;; Finally, the hook
(add-hook 'org-agenda-mode-hook 'my-org-agenda-hl-line)

(use-package display-line-numbers
  :config
  (setq display-line-numbers-width-start t))

(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*"
      uniquify-min-dir-content 2)

(setq use-package-expand-minimally byte-compile-current-file)

;; <backspace> is too far away
(define-key key-translation-map (kbd "C-h") (kbd "DEL"))
(bind-key "C-c h" #'help-command)
(bind-key "C-?" #'help-command)

(advice-add 'elisp-get-fnsym-args-string :around #'ytn-elisp-get-fnsym-args-string)

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

(use-package wdired
  :bind (:map dired-mode-map
              ("r" . wdired-change-to-wdired-mode)))

(use-package sh-script
  :defer t
  :config
  (setq sh-basic-offset 2))

(use-package eldoc
  :defer t
  :delight eldoc-mode)

(use-package isearch
  :config
  :bind (("s-s" . isearch-forward)
         ("s-r" . isearch-backward)
         :map isearch-mode-map
         ("s-s" . isearch-repeat-forward)
         ("s-r" . isearch-repeat-backward)))

(use-package calendar
  :commands calendar-backward-day calendar-backward-week calendar-forward-week calendar-forward-day
  :bind (:map calendar-mode-map
               ("b" . (lambda () (interactive) (calendar-backward-day 1)))
               ("n" . (lambda () (interactive) (calendar-forward-week 1)))
               ("p" . (lambda () (interactive) (calendar-backward-week 1)))
               ("f" . (lambda () (interactive) (calendar-forward-day 1))))
  :config
  (setq calendar-mark-holidays-flag t))

(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "❮")
                                       ("#+END_SRC" . "❯")
                                       ("#+begin_src" . "❮")
                                       ("#+end_src" . "❯")
                                       ("#+BEGIN_EXAMPLE" . "❲")
                                       ("#+END_EXAMPLE" . "❳")
                                       ("#+begin_example" . "❲")
                                       ("#+end_example" . "❳")
                                       ("#+begin_quote" . "„")
                                       ("#+BEGIN_QUOTE" . "„")
                                       ("#+end_quote" . "„")
                                       ("#+END_QUOTE" . "„")
                                       ("[ ]" . "⬜")
                                       ("[X]" . "✔")
                                       ("[-]" . "➖")
                                       (">=" . "≥")
                                       ("=>" . "⇨")))
(setq prettify-symbols-unprettify-at-point 'right-edge)

(setq set-mark-command-repeat-pop t)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
