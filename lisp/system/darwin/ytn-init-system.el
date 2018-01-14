;;; ytn-init-system.el --- System concerns. -*- lexical-binding: t -*-

;; Copyright (C) 2018 Yuto SASAKI
;; Author: Yuto SASAKI <yewton@gmail.com>

;;; Commentary:

;; 

;;; Code:

;; cf. https://apple.stackexchange.com/a/300623
(let ((gls (executable-find "gls")))
  (when gls
    (setq insert-directory-program gls
          dired-listing-switches "-aBhl --group-directories-first")))

(provide 'ytn-init-system)
;;; ytn-init-system.el ends here
