;;; ytn-init-yasnippet.el --- Initialize yasnippet -*- lexical-binding: t -*-

;; Copyright (C) 2018 Yuto SASAKI
;; Author: Yuto SASAKI <yewton@kappa-yewton-home-6.local>

;;; Commentary:

;; Yanisppet (Yet-Another Snippet) initialization.

;;; Code:

(require 'el-get-autoloads)
(require 'no-littering)

(require 'yasnippet)
(require 'yasnippet-snippets)

;;;###autoload
(defun ytn-init-yasnippet ()
  "Initialize yasnippet."
  (unbind-key "<tab>" yas-keymap)
  (setq yas-snippet-dirs `(,(expand-file-name "yasnippet/snippets" no-littering-etc-directory)))
  (yas-global-mode 1)
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)

  (yasnippet-snippets-initialize))

(provide 'ytn-init-yasnippet)
;;; ytn-init-yasnippet.el ends here
