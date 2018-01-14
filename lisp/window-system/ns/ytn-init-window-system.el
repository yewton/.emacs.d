;;; ytn-init-window-system.el --- Window System concerns. -*- lexical-binding: t -*-

;; Copyright (C) 2018 Yuto SASAKI
;; Author: Yuto SASAKI <yewton@gmail.com>

;;; Commentary:

;; 

;;; Code:

(setq locale-coding-system 'utf-8)
(setq system-time-locale "ja_JP.UTF-8")
(setenv "LANG" "ja_JP.UTF-8")
(setenv "LC_ALL" "ja_JP.UTF-8")
(setenv "LC_MESSAGES" "ja_JP.UTF-8")

(setq ns-function-modifier 'hyper)
(setq ns-command-modifier 'meta)
(setq ns-alternate-modifier 'super)

;; cf. https://github.com/milkypostman/powerline/issues/54#issuecomment-65078550
(setq ns-use-srgb-colorspace nil)

;; cf. https://apple.stackexchange.com/a/300623
(let ((gls (executable-find "gls")))
  (when gls
    (setq insert-directory-program gls
          dired-listing-switches "-aBhl --group-directories-first")))


(provide 'ytn-init-window-system)
;;; ytn-init-window-system.el ends here
