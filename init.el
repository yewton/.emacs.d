;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))
(org-babel-load-file (expand-file-name "README.org" user-emacs-directory))
