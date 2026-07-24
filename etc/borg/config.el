;; drone の README.org を texinfo にエクスポートする際 (borg-maketexi)、
;; ソース中の elisp コードブロックを評価しようとして
;; org-confirm-babel-evaluate の確認待ちでバッチが止まるのを防ぐ。
(setq org-export-babel-evaluate nil)
