;;; ytn-hydra.el --- Here be Hydras -*- lexical-binding: t -*-

;; Copyright (C) 2018 Yuto SASAKI
;; Author: Yuto SASAKI <yewton@gmail.com>

;;; Commentary:

;; My dear Hydras.

;;; Code:

(require 'bind-key)

(autoload 'hydra-rectangle/body "ytn-hydra-rectangle" nil t)
(bind-key* "C-x SPC" #'hydra-rectangle/body)

(provide 'ytn-hydra)
;;; ytn-hydra.el ends here
