;;; -*- coding: utf-8; lexical-binding: t -*-
;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(add-to-list 'load-path (expand-file-name "el-get/el-get" user-emacs-directory))
(setq el-get-verbose t)
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(setq el-get-verbose t
      org-id-locations-file (expand-file-name ".org-id-locations" my-data-directory))
(el-get 'sync '(org-mode))

(org-babel-load-file (expand-file-name "README.org" user-emacs-directory))
