;; -*- lexical-binding: t -*-
;; cf. https://apple.stackexchange.com/a/300623
(let ((gls (executable-find "gls")))
  (when gls
    (setq insert-directory-program gls
          dired-listing-switches "-aBhl --group-directories-first")))

(provide 'ytn-init-system-darwin)
;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
