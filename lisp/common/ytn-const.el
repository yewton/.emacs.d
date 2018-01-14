;;; ytn-const.el --- Constant definitions -*- lexical-binding: t -*-

;; Copyright (C) 2018 Yuto SASAKI
;; Author: Yuto SASAKI <yewton@gmail.com>

;;; Commentary:

;; Declare constants.

;;; Code:

(require 'f)

(defconst ytn-var-directory (f-join user-emacs-directory "var"))
(defconst ytn-backup-directory (f-join ytn-var-directory "backup"))

(provide 'ytn-const)
;;; ytn-const.el ends here
