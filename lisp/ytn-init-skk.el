;;; ytn-init-skk.el --- SKK initialization -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Yuto SASAKI
;; Author: Yuto SASAKI <yewton@gmail.com>

;;; Commentary:

;; Initialize SKK.

;;; Code:

(require 'f)
(require 'ccc)
(require 'skk)
(require 'context-skk)
(require 'skk-develop)
(require 'bind-key)
(require 'delight)
(require 'no-littering)

(setq skk-get-jisyo-directory (f-join no-littering-var-directory "skk-get-jisyo"))
(setq skk-sticky-key ";"
      skk-egg-like-newline t
      skk-show-annotation t
      skk-use-jisx0201-input-method t
      skk-auto-insert-paren t
      skk-henkan-strict-okuri-precedence t
      skk-isearch-mode-enable nil
      skk-show-mode-show t
      ;; 'tooltip does not work with `context-skk' :(
      skk-show-mode-style 'inline)

;; Let me `newline-and-indent' in latin-mode!
(advice-add 'skk-setup-latin-mode-map-options :override #'ignore)
(bind-key "C-o" #'skk-kakutei skk-latin-mode-map)
(unbind-key "C-j" skk-latin-mode-map)
(advice-add 'skk-setup-jisx0208-latin-mode-map-options :override #'ignore)
(bind-key "C-o" #'skk-kakutei skk-jisx0208-latin-mode-map)
(unbind-key "C-j" skk-jisx0208-latin-mode-map)

(set-face-attribute 'skk-show-mode-inline-face nil :foreground "black" :background "lemon chiffon")

(delight 'context-skk-mode "" 'context-skk)

(defun ytn-init-skk-extra-jisyo ()
  "Initialize `skk-extra-jisyo-file-list'. Download jisyo files if needed."
  (interactive)
  (if (and (not (f-exists-p skk-get-jisyo-directory))
           (y-or-n-p "Download skk dic files? "))
      (skk-get skk-get-jisyo-directory))
  (dolist (dic (list "SKK-JISYO.L" "SKK-JISYO.lisp"))
    (add-to-list 'skk-extra-jisyo-file-list (f-join skk-get-jisyo-directory dic))))

(add-hook 'emacs-startup-hook #'ytn-init-skk-extra-jisyo)

(provide 'ytn-init-skk)
;;; ytn-init-skk.el ends here
