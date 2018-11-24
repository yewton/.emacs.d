;;; ytn-hydra.el --- Here be Hydras -*- lexical-binding: t -*-

;; Copyright (C) 2018 Yuto SASAKI
;; Author: Yuto SASAKI <yewton@gmail.com>

;;; Commentary:

;; My dear Hydras.

;;; Code:

(require 'bind-key)

(autoload 'hydra-rectangle/body "ytn-hydra-rectangle" nil t)
(bind-key [remap rectangle-mark-mode] #'hydra-rectangle/body)

(defhydra hydra-window-resize (global-map "C-x")
  "Enlarge/shrink window."
  ("{" shrink-window-horizontally)
  ("}" enlarge-window-horizontally)
  ("^" enlarge-window)
  ("&" shrink-window)
  ("+" balance-windows)
  ("-" shrink-window-if-larger-than-buffer)
  ("q" nil)
  ("g" nil))

(provide 'ytn-hydra)
;;; ytn-hydra.el ends here
