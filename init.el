;;; init.el --- My Emacs' configuration entry point. -*- lexical-binding: t -*-

;; Copyright (C) 2016 Yuto SASAKI

;; Author: Yuto SASAKI <yewton@gmail.com>
;; Version: 1.0.0
;; Keywords: convenient

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; README.org を読み込むための最低限の設定を行います。

;;; Code:

;; Without this comment emacs25 adds (package-initialize) here
;; と、spacemacs が言っていたので。
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

(defconst my-data-directory (expand-file-name "data/" user-emacs-directory)
  "バージョン管理外の様々なデータファイル置き場.")
(unless (file-directory-p my-data-directory)
  (make-directory my-data-directory))

(setq org-id-locations-file (expand-file-name ".org-id-locations" my-data-directory))

(declare-function el-get "el-get" (&optional sync &rest packages))
;; org-mode の git clone が泣きたくなる程遅かったので…
(setq el-get-sources '((:name org-mode
                              :type http-tar
                              :options ("xzf")
                              :load-path ("." "contrib/lisp" "lisp")
                              :url "http://orgmode.org/org-9.0.5.tar.gz")))
(el-get 'sync '(org-mode))

(org-babel-load-file (expand-file-name "README.org" user-emacs-directory))
;;; init.el ends here
