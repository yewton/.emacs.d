;;; ytn-init-window-system-x.el --- Window System concerns. -*- lexical-binding: t -*-

;; Copyright (C) 2018 Yuto SASAKI
;; Author: Yuto SASAKI <yewton@gmail.com>

;;; Commentary:

;; 

;;; Code:

(let* ((fonts '(
                ("Myrica M" . 120)
                ("Monospace" . 96)
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

(provide 'ytn-init-window-system-x)
;;; ytn-init-window-system-x.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
