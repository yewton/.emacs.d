;;; ytn-init-spaceline.el --- Spaceline initialization -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Yuto SASAKI
;; Author: Yuto SASAKI <yewton@gmail.com>

;;; Commentary:

;; Initialize Spaceline.

;;; Code:

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

(setq-default mode-line-format '("%e" (:eval (spaceline-ml-main))))

(ytn-spaceline-theme)

(provide 'ytn-init-spaceline)
;;; ytn-init-spaceline.el ends here
