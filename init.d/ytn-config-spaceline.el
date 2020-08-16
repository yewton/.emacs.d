;; -*- lexical-binding: t; -*-
(require 'spaceline-segments)
(require 'ccc)
(require 'skk)

(setq spaceline-window-numbers-unicode t
      spaceline-workspace-numbers-unicode t
      spaceline-minor-modes-separator " ")

(spaceline-define-segment skk
  "Show current SKK status."
  (cond
   (skk-abbrev-mode skk-abbrev-mode-string)
   (skk-jisx0208-latin-mode skk-jisx0208-latin-mode-string)
   (skk-katakana skk-katakana-mode-string)
   (skk-j-mode skk-hiragana-mode-string)
   (skk-jisx0201-mode skk-jisx0201-mode-string)
   (skk-latin-mode skk-latin-mode-string)
   (t "--")))

(advice-add 'skk-setup-modeline :override #'ignore)

(defun ytn-spaceline-theme ()
  "Install the modeline used by yewton."
  (spaceline-compile
    '(((persp-name workspace-number window-number)
       :fallback evil-state
       :face highlight-face
       :priority 0)
      (anzu :priority 4)
      auto-compile
      skk
      ((buffer-modified buffer-size buffer-id remote-host)
       :priority 5)
      (process :when active)
      ((flycheck-error flycheck-warning flycheck-info)
       :when active
       :priority 3)
      (minor-modes :when active)
      (mu4e-alert-segment :when active)
      (erc-track :when active)
      (version-control :when active :priority 7)
      (org-pomodoro :when active)
      (org-clock :when active)
      nyan-cat)

    '(which-function
      (python-pyvenv :fallback python-pyenv)
      purpose
      (battery :when active)
      (selection-info :priority 2)
      input-method
      ((buffer-encoding-abbrev
        point-position
        line-column)
       :separator " | "
       :priority 3)
      (global :when active)
      (buffer-position :priority 0)
      (hud :priority 0))))

;;; Hack

;; see https://github.com/tarsius/moody/commit/8f96f1ec6b331747774d84a6cc49968503bac7d0
(defun powerline-set-selected-window1 (&optional _)
  (if (and (fboundp 'frame-focus-state) (with-no-warnings (null (frame-focus-state))))
    (setq powerline-selected-window nil)
    (let ((win (selected-window)))
      (unless (minibuffer-window-active-p win)
        (setq powerline-selected-window win)))))
(add-hook 'pre-redisplay-functions #'powerline-set-selected-window1)
(with-no-warnings
  (if (boundp 'after-focus-change-function)
      (add-function :after after-focus-change-function #'powerline-set-selected-window1)
    (add-hook 'focus-out-hook #'powerline-set-selected-window)
    (add-hook 'focus-in-hook #'powerline-set-selected-window)))

;; see https://github.com/tarsius/moody/commit/a7fb64d6fae15ed6ff87e540ff177134fc0b19b5
(defvar-local powerline--size-hacked-p nil)
(defun powerline-redisplay (&optional _force &rest _ignored)
  "See `moody-redisplay'"
  (when (and mode-line-format (not powerline--size-hacked-p))
    (setq powerline--size-hacked-p t)
    (redisplay t)))
(advice-add 'fit-window-to-buffer :before #'powerline-redisplay)

(setq-default mode-line-format '("%e" (:eval (spaceline-ml-main))))

(remove-hook 'window-configuration-change-hook #'powerline-set-selected-window)
(remove-hook 'buffer-list-update-hook #'powerline-set-selected-window)
(ad-disable-advice 'handle-switch-frame 'after 'powerline-handle-switch-frame)
;; https://github.com/TheBB/spaceline/issues/110#issuecomment-283513541
(with-no-warnings
  (remove-hook 'focus-out-hook #'powerline-unset-selected-window)
  (remove-hook 'focus-in-hook #'powerline-set-selected-window))

(add-hook 'window-setup-hook #'ytn-spaceline-theme)
;; macOS だと起動直後にフォーカスしていても `(frame-focus-state)' が nil になっているので暫定対応
(add-hook 'window-setup-hook #'powerline-set-selected-window)
