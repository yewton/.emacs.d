;; -*- lexical-binding: t -*-
(require 'el-get)
(require 'exec-path-from-shell)
(require 'no-littering)

;;;###autoload
(defun ytn-init-install-packages ()
  (interactive)
  ;; to avoid build errors due to PATH
  (when (memq window-system '(ns x))
    (exec-path-from-shell-initialize))
  (el-get 'sync (mapcar #'el-get-source-name el-get-sources)))

(provide 'ytn-init-install-packages)
