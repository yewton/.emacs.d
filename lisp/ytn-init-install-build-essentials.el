;; -*- lexical-binding: t -*-
(require 'el-get)

;;;###autoload
(defun ytn-init-install-build-essentials ()
  "Configure el-get and install build-essential packages."
  (add-to-list 'el-get-recipe-path
               (expand-file-name (convert-standard-filename "etc/el-get-recipes/") user-emacs-directory))
  (let ((el-get-use-autoloads nil))
    (el-get 'sync '(exec-path-from-shell no-littering auto-compile))))

(provide 'ytn-init-install-build-essentials)
;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
