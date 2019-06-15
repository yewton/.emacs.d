;; -*- lexical-binding: t; -*-
(eval-when-compile (require 'use-package))

;;;###autoload
(defun ytn-init-configure-org-mode()
  (use-package org
    :commands (org-clock-persistence-insinuate)
    :bind (("C-c c" . org-capture)
           ("C-c l" . org-store-link))
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
          org-cycle-separator-lines 0))

  (use-package org-goto
    :config
    (setq org-goto-auto-isearch nil))

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
          org-agenda-clockreport-parameter-plist '(:lang "ja" :link t :level t :timestamp t)))

  (use-package org-attach
    :config
    (setq org-attach-store-link-p t))

  (use-package org-clock
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
    :after (org)))

(provide 'ytn-init-configure-org-mode)
;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
