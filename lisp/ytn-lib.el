;; -*- lexical-binding: t -*-
(require 'f)

(defvar ytn-init-dir (f-expand "init.d" user-emacs-directory))

;;;###autoload
(defun ytn-load-init-file (file)
  (load (f-expand file ytn-init-dir)))

(provide 'ytn-lib)
;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
