;; -*- lexical-binding: t -*-
(require 'el-get)
(require 'exec-path-from-shell)
(require 'no-littering)

(require 'ytn-recipes)

(setq el-get-sources ytn-recipes)

;;;###autoload
(defun ytn-init-install-packages ()
  (interactive)
  ;; to avoid build errors due to PATH
  (when (memq window-system '(ns x))
    (exec-path-from-shell-initialize))

  (let ((el-get-sources ytn-recipes))
    (el-get 'sync (mapcar #'el-get-source-name el-get-sources))))

(provide 'ytn-init-install-packages)
;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
