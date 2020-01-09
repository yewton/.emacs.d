;; -*- lexical-binding: t; -*-
(eval-when-compile (require 'use-package))

(use-package org
  :commands (org-clock-persistence-insinuate)
  :bind (("C-c l" . org-store-link)
         ("C-c b" . org-switchb))
  :config
  (org-clock-persistence-insinuate)
  (setq org-ellipsis " …"
        org-fontify-whole-heading-line t
        org-startup-indented t
        org-startup-with-inline-images t
        org-startup-folded 'content
        org-log-redeadline 'time
        org-log-reschedule 'time
        org-reverse-note-order t
        org-catch-invisible-edits 'show-and-error
        org-cycle-separator-lines 0
        org-outline-path-complete-in-steps nil
        org-agenda-files '("~/org/agenda")
        org-refile-use-outline-path 'file
        org-use-speed-commands t
        org-image-actual-width nil
        org-refile-targets '((nil :maxlevel . 3) (org-agenda-files :maxlevel . 1))))

(use-package org-goto
  :defer t
  :config
  (setq org-goto-auto-isearch nil
        org-goto-interface 'outline-path-completion))

(use-package org-agenda
  :bind (("C-c a" . org-agenda))
  :config
  (set-face-underline 'org-agenda-current-time nil)
  (setq org-agenda-window-setup 'reorganize-frame
        org-agenda-restore-windows-after-quit t
        org-agenda-start-on-weekday 1
        org-agenda-deadline-leaders '("⏰" "🔜%2d日後" "⚠%2d日超過")
        org-agenda-scheduled-leaders '("📅" "📌%2d日前")
        org-agenda-format-date "%_2m月%e日(%a) %_20Y年"
        org-agenda-show-current-time-in-grid t
        org-agenda-current-time-string "▷ - - - - - - - - - - - - - - - - - - - - - - - - -"
        org-agenda-compact-blocks t
        org-agenda-show-outline-path nil
        org-agenda-sticky nil
        org-agenda-custom-commands '(("n" "Agenda and all TODOs" ((agenda "") (alltodo "")))
                                     ("u" "Unscheduled TODO" todo ""
                                      ((org-agenda-overriding-header "Unscheduled TODO")
                                       (org-agenda-todo-ignore-scheduled 'all)
                                       (org-agenda-todo-ignore-deadlines 'all)))
                                     ("N" "Non-recurring TODO" tags-todo "-recurring-routine"))
        org-agenda-clockreport-parameter-plist '(:hidefiles t :properties ("CATEGORY") :lang "ja" :link t :compact t :stepskip0 t :fileskip0 t)
        org-agenda-log-mode-items '(clock)))

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
  (setq org-attach-store-link-p t))

(use-package ox
  :defer t
  :config
  (setq org-export-default-language "ja"))

(use-package org-clock
  :defer t
  :config
  (setq org-clock-clocked-in-display 'both
        org-clock-persist t)
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
  (setq org-download-annotate-function
        (lambda (link)
          (format "#+DOWNLOADED: %s @ %s\n#+ATTR_ORG: :width 500\n"
                  (if (equal link org-download-screenshot-file)
                      "screenshot"
                    link)
                  (format-time-string "%Y-%m-%d %H:%M:%S"))))
  (add-hook 'dired-mode-hook 'org-download-enable))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
