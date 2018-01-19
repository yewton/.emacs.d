;;; ytn-init-el-get.el --- Initialize el-get -*- lexical-binding: t -*-

;; Copyright (C) 2018 Yuto SASAKI
;; Author: Yuto SASAKI <yewton@kappa-yewton-home.local>

;;; Commentary:

;; Configure el-get and install build-essential packages.

;;; Code:

(require 'el-get)

;;;###autoload
(defun ytn-init-el-get ()
  "Configure el-get and install build-essential packages."
  (add-to-list 'el-get-recipe-path
               (expand-file-name (convert-standard-filename "etc/el-get-recipes/") user-emacs-directory))
  (let ((el-get-use-autoloads nil))
    (el-get 'sync '(exec-path-from-shell no-littering))))

(provide 'ytn-init-el-get)
;;; ytn-init-el-get.el ends here
