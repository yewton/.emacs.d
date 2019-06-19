;; -*- lexical-binding: t; -*-

;; cf. https://www.reddit.com/r/emacs/comments/56fvgd/is_there_a_way_to_stop_emacs_from_adding_the/
(setq package--init-file-ensured t)

;; https://emacs.stackexchange.com/a/33523/18118
(defun sh/current-time-microseconds ()
  "Return the current time formatted to include microseconds."
  (let* ((nowtime (current-time))
         (now-ms (nth 2 nowtime)))
    (concat (format-time-string "[%Y-%m-%dT%T" nowtime) (format ".%d]" now-ms))))

(defun sh/ad-timestamp-message (FORMAT-STRING &rest args)
  "Advice to run before `message' that prepends a timestamp to each message.

Activate this advice with:
(advice-add 'message :before 'sh/ad-timestamp-message)"
  (unless (string-equal FORMAT-STRING "%s%s")
    (let ((deactivate-mark nil)
          (inhibit-read-only t))
      (with-current-buffer "*Messages*"
        (goto-char (point-max))
        (if (not (bolp))
          (newline))
        (insert (sh/current-time-microseconds) " ")))))

(advice-add 'message :before 'sh/ad-timestamp-message)

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(defvar ytn-lisp-dir (expand-file-name "lisp" user-emacs-directory))
(defvar ytn-init-dir (expand-file-name "init.d" user-emacs-directory))

(require 'files) ; for make-backup-files
(require 'autoload)

(defun ytn-update-autoloads ()
  (interactive)
  (let ((generated-autoload-file (expand-file-name "ytn-init-autoloads.el" ytn-lisp-dir))
        (make-backup-files nil))
    (update-directory-autoloads ytn-lisp-dir)))

(add-to-list 'load-path ytn-lisp-dir)

(ytn-update-autoloads)
(require 'ytn-init-autoloads)

(ytn-init-install-el-get)
(ytn-init-set-el-get-sources)

(ytn-init-install-build-essentials)

(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

(ytn-init-install-packages)

(byte-recompile-directory ytn-lisp-dir 0)
(byte-recompile-directory ytn-init-dir 0)

(ytn-load-init-file "config")

(load custom-file 'noerror)

;; Local Variables:
;; no-byte-compile: t
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
