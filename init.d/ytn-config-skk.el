;;-*- lexical-binding: t; -*-
(require 'f)
(require 'delight)
(require 'no-littering)
(require 'skk-vars)

(eval-when-compile (require 'use-package))

(autoload 'skk-get "skk-develop" "" nil)

(defun ytn-init-skk-extra-jisyo ()
  (interactive)
  (when (and (not (f-exists-p skk-get-jisyo-directory))
             (y-or-n-p "Download skk dic files? "))
    (skk-get skk-get-jisyo-directory))
  (dolist (dic (list "SKK-JISYO.L" "SKK-JISYO.lisp"))
    (add-to-list 'skk-extra-jisyo-file-list (f-join skk-get-jisyo-directory dic))))

(use-package skk-vars
  :config
  (setq skk-sticky-key ";"
        skk-egg-like-newline t
        skk-show-annotation t
        skk-use-jisx0201-input-method t
        skk-henkan-strict-okuri-precedence t
        skk-isearch-mode-enable nil
        skk-show-mode-show t
        ;; 'tooltip does not work with `context-skk' :(
        skk-show-mode-style 'inline
        skk-get-jisyo-directory (f-join no-littering-var-directory "skk-get-jisyo"))

  (set-face-attribute 'skk-show-mode-inline-face nil :foreground "black" :background "lemon chiffon")

  (use-package context-skk
    :delight context-skk-mode)

  (use-package skk
    :bind* (("C-x C-j" . skk-mode))
    :commands (skk-kakutei)
    :config
    ;; Let me `newline-and-indent' in latin-mode!
    (advice-add 'skk-setup-latin-mode-map-options :override #'ignore)
    (bind-key "C-o" #'skk-kakutei skk-latin-mode-map)
    (unbind-key "C-j" skk-latin-mode-map)
    (advice-add 'skk-setup-jisx0208-latin-mode-map-options :override #'ignore)
    (bind-key "C-o" #'skk-kakutei skk-jisx0208-latin-mode-map)
    (unbind-key "C-j" skk-jisx0208-latin-mode-map)
    (use-package markdown-mode
      :defer t
      :config
      (skk-wrap-newline-command markdown-enter-key))

    (add-hook 'emacs-startup-hook #'ytn-init-skk-extra-jisyo)))
