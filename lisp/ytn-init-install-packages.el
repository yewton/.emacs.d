;;; ytn-init-install-packages.el --- Install packages with el-get. -*- lexical-binding: t -*-

;; Copyright (C) 2018 Yuto SASAKI
;; Author: Yuto SASAKI <yewton@kappa-yewton-home.local>

;;; Commentary:

;; In

;;; Code:

(require 'el-get)
(require 'exec-path-from-shell)
(require 'no-littering)

;;;###autoload
(defun ytn-init-install-packages ()
  "Install all packages defined in `el-get-sources'."
  ;; to avoid build errors due to PATH
  (when (memq window-system '(ns x))
    (exec-path-from-shell-initialize))

  (el-get 'sync (mapcar #'el-get-source-name el-get-sources)))

(provide 'ytn-init-install-packages)
;;; ytn-init-install-packages.el ends here
