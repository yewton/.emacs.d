;; -*- lexical-binding: t -*-
(require 'bind-key)
(require 'hydra)
(require 'ytn-init-autoloads)

(bind-key [remap rectangle-mark-mode] #'hydra-rectangle/body)

;;;###autoload
(defun ytn-install-hydra ()
  (defhydra hydra-window-resize (global-map "C-x")
    "Enlarge/shrink window."
    ("{" shrink-window-horizontally)
    ("}" enlarge-window-horizontally)
    ("^" enlarge-window)
    ("&" shrink-window)
    ("+" balance-windows)
    ("-" shrink-window-if-larger-than-buffer)
    ("q" nil)
    ("g" nil)))

(provide 'ytn-install-hydra)
;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
