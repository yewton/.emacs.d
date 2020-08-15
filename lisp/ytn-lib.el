;; -*- lexical-binding: t -*-
(defvar ytn-lisp-dir (expand-file-name "lisp" user-emacs-directory))
(defvar ytn-init-dir (expand-file-name "init.d" user-emacs-directory))

;; https://emacs.stackexchange.com/a/33523/18118
(defun ytn-current-time-microseconds ()
  "Return the current time formatted to include microseconds."
  (let* ((nowtime (current-time))
         (now-ms (nth 2 nowtime)))
    (concat (format-time-string "[%Y-%m-%dT%T" nowtime) (format ".%d]" now-ms))))

(defun ytn-ad-timestamp-message (FORMAT-STRING &rest _args)
  "Advice to run before `message' that prepends a timestamp to each message.

Activate this advice with:
(advice-add 'message :before 'ytn-ad-timestamp-message)"
  (unless (string-equal FORMAT-STRING "%s%s")
    (let ((deactivate-mark nil)
          (inhibit-read-only t))
      (with-current-buffer "*Messages*"
        (goto-char (point-max))
        (if (not (bolp))
          (newline))
        (insert (ytn-current-time-microseconds) " ")))))

(provide 'ytn-lib)
