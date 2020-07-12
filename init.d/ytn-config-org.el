;; -*- lexical-binding: t; -*-
(eval-when-compile (require 'use-package))

(require 'f)
(require 's)
(require 'avy)

(use-package org
  :commands (org-clock-persistence-insinuate
             org-eval-in-calendar)
  :bind (("C-c l" . org-store-link)
         ("C-c b" . org-switchb)
         (:map org-mode-map
               ("C-'" . avy-goto-char-2))
         (:map org-read-date-minibuffer-local-map
               ("<up>" . (lambda () (interactive)
                           (org-eval-in-calendar '(calendar-backward-week 1))))
               ("<down>" . (lambda () (interactive)
                            (org-eval-in-calendar '(calendar-forward-week 1))))
               ("<left>" . (lambda () (interactive)
                             (org-eval-in-calendar '(calendar-backward-day 1))))
               ("<right>" . (lambda () (interactive)
                              (org-eval-in-calendar '(calendar-forward-day 1))))
               ("b" . (lambda () (interactive)
                        (org-eval-in-calendar '(calendar-backward-day 1))))
               ("n" . (lambda () (interactive)
                        (org-eval-in-calendar '(calendar-forward-week 1))))
               ("p" . (lambda () (interactive)
                           (org-eval-in-calendar '(calendar-backward-week 1))))
               ("f" . (lambda () (interactive)
                        (org-eval-in-calendar '(calendar-forward-day 1))))))
  :hook ((org-mode . visual-line-mode)
         (org-mode . prettify-symbols-mode))
  :config
  (org-clock-persistence-insinuate)
  (setq org-ellipsis "⤵")
  (setq org-fontify-whole-heading-line t)
  (setq org-startup-indented t)
  (setq org-startup-with-inline-images t)
  (setq org-startup-folded 'content)
  (setq org-log-redeadline 'time)
  (setq org-log-reschedule 'time)
  (setq org-reverse-note-order t)
  (setq org-catch-invisible-edits 'show-and-error)
  (setq org-cycle-separator-lines 0)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-agenda-files '("~/org/agenda"))
  (setq org-refile-use-outline-path 'file)
  (setq org-use-speed-commands t)
  (setq org-image-actual-width nil)
  (setq org-refile-targets '((nil :maxlevel . 3) (org-agenda-files :maxlevel . 1)))
  (setq org-src-tab-acts-natively t)
  (setq org-hide-emphasis-markers t)
  (setq org-fontify-done-headline t)
  (setq org-hide-leading-stars t)
  (setq org-pretty-entities t)
  (setq org-odd-levels-only nil)
  (setq org-log-into-drawer t)
  (setq org-use-sub-superscripts nil))

(use-package org-goto
  :defer t
  :config
  (setq org-goto-auto-isearch nil
        org-goto-interface 'outline-path-completion))

(use-package org-agenda
  :bind (("C-c a" . org-agenda))
  :config
  (set-face-underline 'org-agenda-current-time nil)
  (setq org-agenda-window-setup 'reorganize-frame)
  (setq org-agenda-restore-windows-after-quit t)
  (setq org-agenda-start-on-weekday 1)
  (setq org-agenda-deadline-leaders '("⏰" "🔜%2d日後" "⚠%2d日超過"))
  (setq org-agenda-scheduled-leaders '("📅" "📌%2d日前"))
  (setq org-agenda-format-date "%-m月%e日(%a) %_20Y年")
  (setq org-agenda-show-current-time-in-grid t)
  (setq org-agenda-current-time-string "▷ - - - - - - - - - - - - - - - - - - - - - - - - -")
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-show-outline-path nil)
  (setq org-agenda-sticky nil)
  (setq org-agenda-include-diary nil)
  (setq org-sort-agenda-notime-is-late nil)
  (setq org-agenda-custom-commands '(("n" "Agenda and all TODOs" ((agenda "") (alltodo "")))
                                     ("u" "Unscheduled TODO" todo ""
                                      ((org-agenda-overriding-header "Unscheduled TODO")
                                       (org-agenda-todo-ignore-scheduled 'all)
                                       (org-agenda-todo-ignore-deadlines 'all)))
                                     ("N" "Non-recurring TODO" todo ""
                                      ((org-agenda-overriding-header "Non-recurring TODO")
                                       (org-agenda-category-filter-preset '("-routine"))))))
  (setq org-agenda-breadcrumbs-separator " ❱ ")
  (setq org-agenda-prefix-format '((agenda . " %i %?-12t% s")
                                   (todo . " %i% s%?b")
                                   (tags . " %i")
                                   (search . " %i")))
  (setq org-agenda-clockreport-parameter-plist '(:hidefiles t :properties ("CATEGORY") :lang "ja" :link t :compact t :stepskip0 t :fileskip0 t)
        org-agenda-log-mode-items '(clock))
  (let ((fn (lambda (category)
              `(,category ,(f-join user-emacs-directory "res" (s-concat category ".svg")) nil nil :ascent center))))

    (setq org-agenda-category-icon-alist `(,(funcall fn "task")
                                           ,(funcall fn "routine")
                                           ,(funcall fn "event")
                                           ,(funcall fn "leave")
                                           ,(funcall fn "anniv")
                                           ,(funcall fn "log")
                                           ,(funcall fn "holiday")
                                           ,(funcall fn "sprint")))))

(use-package org-capture
  :config
  (setq org-capture-templates '(("t" "ToDo" entry (file+headline "" "Inbox")
                                 "* TODO %?\n%U" :empty-lines 1 :prepend t)
                                ("j" "Jot" entry (file+headline "" "Inbox")
                                 "* on %<%Y-%m-%d %H:%M:%S>\n%U\n\n%?" :empty-lines 1 :prepend t)
                                ("e" "Entry" entry (file+headline "" "Inbox")
                                 "* %?\n%U" :empty-lines 1 :prepend t)
                                ("i" "Clock-in" entry (file+headline "" "Inbox")
                                 "* %?\n%U\n" :clock-in t :clock-keep t :empty-lines 1 :prepend t))))


(use-package org-attach
  :defer t
  :config
  (setq org-attach-store-link-p t)
  (setq org-attach-dir-relative t)
  (setq org-attach-use-inheritance t))

(use-package ox
  :defer t
  :config
  (setq org-export-default-language "ja"))

(use-package org-clock
  :defer t
  :config
  (setq org-clock-clocked-in-display 'both)
  (setq org-clock-persist t)
  (add-to-list 'org-clock-clocktable-language-setup
               '("ja" "ファイル" "レベル" "タイムスタンプ" "見出し" "工数" "全て" "合計" "ファイル計" "Clock summary at")))

(use-package org-mru-clock
  :bind* (("C-c C-x C-x" . org-mru-clock-in)
          ("C-c C-x C-j" . org-mru-clock-select-recent-task))
  :init
  (setq org-mru-clock-how-many 10)
  (use-package ivy
    :commands (ivy-completing-read)
    :config
    (setq org-mru-clock-completing-read #'ivy-completing-read)))

(use-package org-tempo
  :after (org))

(use-package org-duration
  :defer t
  :config
  (setq org-duration-format '((special . h:mm))))

(use-package ox-hugo
  :after ox
  :init
  (defalias 'toml-mode 'conf-toml-mode)
  :config
  (setq org-hugo-use-code-for-kbd t))

(use-package org-download
  :after org
  :functions (org-link-escape)
  :config
  (setq org-download-method 'attach)
  ;; ox-hugo が attachment: link abbrev に対応していないため
  (setq org-download-link-format-function
        (lambda (filename)
          (format org-download-link-format
                  (org-link-escape
                   (funcall org-download-abbreviate-filename-function filename)))))
  (setq org-download-annotate-function (lambda (_link) "#+ATTR_ORG: :width 500\n"))
  (setq org-download-screenshot-method (cond ((eq system-type 'darwin) "screencapture -i %s")))
  (add-hook 'dired-mode-hook 'org-download-enable))

(use-package org-protocol
  :demand t)

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("◉" "◈" "○" "◇" "▷")))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
