;;; ytn-init-bootstrap.el --- Bootstrapping -*- lexical-binding: t -*-

;; Copyright (C) 2018 Yuto SASAKI

;; Author: Yuto SASAKI <yewton@kappa-yewton-home.local>
;; Version: 1.0.0

;;; Commentary:

;; Install el-get.

;;; Code:

(require 'gnutls)
(require 'ytn-recipes)

(defvar el-get-install-skip-emacswiki-recipes)
(defvar el-get-install-shallow-clone)
(defvar el-get-verbose)
(defvar el-get-dir)

;;;###autoload
(defun ytn-init-bootstrap ()
  "Install el-get."
  (interactive)
  (setq load-prefer-newer t
        package-user-dir (expand-file-name (convert-standard-filename "var/elpa") user-emacs-directory)
        el-get-verbose t
        el-get-dir (expand-file-name (convert-standard-filename "var/el-get") user-emacs-directory))

  ;; cf. https://emacs.stackexchange.com/a/35953/18118
  (add-to-list 'gnutls-trustfiles "/private/etc/ssl/cert.pem")
  (add-to-list 'load-path (expand-file-name "el-get" el-get-dir))

  (unless (require 'el-get nil t)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/yewton/el-get/fa6ee7cd5e19952a3a636fb5bb0ad18491845db4/el-get-install.el")
      (let (el-get-install-skip-emacswiki-recipes
            el-get-install-shallow-clone)
        (goto-char (point-max))
        (eval-print-last-sexp)))))

(provide 'ytn-init-bootstrap)
;;; ytn-init-bootstrap.el ends here