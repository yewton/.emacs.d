;; -*- lexical-binding: t -*-
(require 'gnutls)

(defvar el-get-install-skip-emacswiki-recipes)
(defvar el-get-install-shallow-clone)
(defvar el-get-verbose)
(defvar el-get-dir)
(defvar el-get-autoload-file)

;;;###autoload
(defun ytn-init-install-el-get ()
  (interactive)
  (setq package-user-dir (expand-file-name (convert-standard-filename "var/elpa") user-emacs-directory)
        el-get-verbose t
        el-get-dir (expand-file-name (convert-standard-filename "var/el-get") user-emacs-directory)
        el-get-autoload-file (expand-file-name "el-get-autoloads.el" el-get-dir))

  ;; cf. https://emacs.stackexchange.com/a/35953/18118
  (add-to-list 'gnutls-trustfiles "/private/etc/ssl/cert.pem")
  (add-to-list 'load-path (expand-file-name "el-get" el-get-dir))

  (unless (require 'el-get nil t)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
      (let (el-get-install-skip-emacswiki-recipes
            el-get-install-shallow-clone)
        (goto-char (point-max))
        (eval-print-last-sexp)))))

(provide 'ytn-init-install-el-get)
;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
