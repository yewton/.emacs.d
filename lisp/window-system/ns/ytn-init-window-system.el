;;; ytn-init-window-system.el --- Window System concerns. -*- lexical-binding: t -*-

;; Copyright (C) 2018 Yuto SASAKI
;; Author: Yuto SASAKI <yewton@gmail.com>

;;; Commentary:

n;; 

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

(add-to-list 'face-font-rescale-alist '(".*Hiragino.*" . 1.2))
(let* ((fonts '(
                ("Ricty Discord for Powerline" . 140)
                ("Myrica M" . 140)
                ("Ricty Discord" . 135)
                ("MyricaM M" . 140)
                ("Menlo" . 120)
                ))
       (font (cl-find-if (lambda (f) (find-font (font-spec :name (car f))))
                         fonts)))
  (when font
    (let ((font-family (car font))
          (font-height (cdr font)))
      (set-face-attribute 'default nil
                          :family font-family
                          :height font-height)
      (dolist (face '(variable-pitch fixed-pitch))
        (set-face-attribute face nil
                            :family font-family))
      (dolist (charset '(japanese-jisx0208 japanese-jisx0208 katakana-jisx0201))
        (set-fontset-font (frame-parameter nil 'font)
                          charset
                          (font-spec :family font-family))))))

(provide 'ytn-init-window-system)
;;; ytn-init-window-system.el ends here
