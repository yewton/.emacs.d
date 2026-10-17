;;; warning-monitor.el --- Classify and report CI diagnostics -*- lexical-binding: t; -*-

(require 'bytecomp)
(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defconst toncs-check-root
  (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(defconst toncs-check-monitor-file (file-truename load-file-name))
(defvar toncs-check-diagnostics nil
  "Diagnostic plists with severity, origin, source file, and message.")
(defvar toncs-check-label (or (getenv "TONCS_CHECK_LABEL") "check"))
(defvar toncs-check-compiling-file nil)

(defun toncs-check-origin (file)
  "Classify FILE as upstream, configuration, or unknown."
  (cond
   ((not (stringp file)) 'unknown)
   ((file-in-directory-p file (expand-file-name "lib" toncs-check-root)) 'upstream)
   ((file-in-directory-p file toncs-check-root) 'configuration)
   (t 'unknown)))

(defun toncs-check-add (severity file message)
  "Record MESSAGE at SEVERITY and classify its source FILE."
  (let ((entry (list :severity severity :origin (toncs-check-origin file)
                     :file (and file (file-relative-name file toncs-check-root))
                     :message message)))
    ;; Emacs can replay the same delayed warning after startup.
    (unless (member entry toncs-check-diagnostics)
      (push entry toncs-check-diagnostics)
      (message "CHECK: %s %s %s: %s" severity (plist-get entry :origin)
               (or (plist-get entry :file) "unknown source") message))))

(defun toncs-check-record (format-string &rest args)
  "Record a fatal check failure described by FORMAT-STRING and ARGS."
  (toncs-check-add 'error nil (apply #'format format-string args)))

(defun toncs-check-byte-warning (text &optional _fill level)
  "Record compiler TEXT at LEVEL, retaining its source file."
  (toncs-check-add (if (memq level '(:error error :emergency emergency)) 'error 'warning)
                   (or byte-compile-current-file toncs-check-compiling-file) text))

(defun toncs-check-compile (function file &rest args)
  "Call compiler FUNCTION for FILE with ARGS and record failures.
Enable all warnings. Only upstream warnings may pass the check."
  (let ((byte-compile-warnings t)
        (byte-compile-error-on-warn nil)
        (toncs-check-compiling-file file))
    (condition-case err
        (let ((result (apply function file args)))
          (unless result
            (toncs-check-record "Compilation failed: %s" file))
          result)
      (error
       (toncs-check-record "Compilation raised an error in %s: %s"
                           file (error-message-string err))
       (signal (car err) (cdr err))))))

(defun toncs-check-warning-file (type)
  "Find the emitting source for warning TYPE, leaving uncertain cases unknown."
  (or
   ;; The loader includes the exact source in missing-cookie warning types.
   (and (consp type) (eq (car type) 'files)
        (eq (cadr type) 'missing-lexbind-cookie) (stringp (caddr type))
        (caddr type))
   (catch 'source
     (dolist (frame (backtrace-frames))
       (let ((function (nth 1 frame)))
         ;; Top-level forms have no named caller. Stop at the load boundary
         ;; instead of attributing them to an outer Borg or test caller.
         (when (memq function '(load load-with-code-conversion require))
           (throw 'source load-file-name))
         (when (symbolp function)
           (let ((file (symbol-file function 'defun)))
             (when (and file
                        (not (eq (toncs-check-origin file) 'unknown))
                        (not (equal (file-truename file) toncs-check-monitor-file))
                        (not (string-prefix-p "toncs-startup-check-"
                                              (symbol-name function))))
               (throw 'source file))))))
     load-file-name)))

(defun toncs-check-warning (type text &optional level &rest _args)
  "Record runtime warning TYPE with TEXT and LEVEL before display filtering."
  (unless (and toncs-check-compiling-file
               (eq (if (consp type) (car type) type) 'bytecomp))
    (when (memq (or level :warning) '(:warning :error :emergency))
      (let* ((severity (if (memq level '(:error :emergency)) 'error 'warning))
             (file (toncs-check-warning-file type))
             (message (format "Runtime %S: %s" type text))
             (replayed-text (replace-regexp-in-string " \\[[0-9]+ times\\]\\'" "" text))
             (bytecomp (eq (if (consp type) (car type) type) 'bytecomp)))
        ;; Startup replays delayed warnings after their load context is gone.
        ;; Keep the diagnostic recorded at emission, including its provenance.
        ;; Only the built-in replay path may reuse that original attribution.
        (unless (and (null file)
                     (cl-some (lambda (frame)
                                (eq (nth 1 frame) 'display-delayed-warnings))
                              (backtrace-frames))
                     (cl-some (lambda (entry)
                                (and (eq (plist-get entry :severity) severity)
                                     (or (equal (plist-get entry :message)
                                                (format "Runtime %S: %s" type replayed-text))
                                         (and bytecomp
                                              (equal (plist-get entry :message)
                                                     replayed-text)))))
                              toncs-check-diagnostics))
          (toncs-check-add severity file message))))))

(advice-add 'byte-compile-log-warning :before #'toncs-check-byte-warning)
(advice-add 'byte-compile-file :around #'toncs-check-compile)
(advice-add 'display-warning :before #'toncs-check-warning)

(defun toncs-check-fatal-p (entry)
  "Return non-nil unless ENTRY is an attributable upstream warning."
  (not (and (eq (plist-get entry :severity) 'warning)
            (eq (plist-get entry :origin) 'upstream))))

(defun toncs-check-diagnostic-text ()
  "Render all collected diagnostics as readable text."
  (mapconcat
   (lambda (entry)
     (format "%s [%s] %s: %s"
             (plist-get entry :severity) (plist-get entry :origin)
             (or (plist-get entry :file) "unknown source") (plist-get entry :message)))
   (reverse toncs-check-diagnostics) "\n"))

(defun toncs-check-escape-command (text)
  "Escape TEXT for a GitHub Actions annotation message."
  (dolist (pair '(("%" . "%25") ("\r" . "%0D") ("\n" . "%0A")) text)
    (setq text (string-replace (car pair) (cdr pair) text))))

(defun toncs-check-annotation-text ()
  "Render GitHub warning/error annotations for the collected diagnostics."
  (if (not (equal (getenv "GITHUB_ACTIONS") "true")) ""
    (mapconcat
     (lambda (entry)
       (format "::%s::%s"
               (if (toncs-check-fatal-p entry) "error" "warning")
               (toncs-check-escape-command
                (format "[%s] %s: %s" (plist-get entry :origin)
                        (or (plist-get entry :file) "unknown source")
                        (plist-get entry :message)))))
     (reverse toncs-check-diagnostics) "\n")))

(defun toncs-check-write-reports ()
  "Write structured diagnostics and an Actions step summary."
  (let* ((fatal (cl-count-if #'toncs-check-fatal-p toncs-check-diagnostics))
         (upstream (- (length toncs-check-diagnostics) fatal))
         (output (getenv "TONCS_CHECK_OUTPUT"))
         (summary (getenv "GITHUB_STEP_SUMMARY")))
    (when output
      (make-directory output t)
      (with-temp-file (expand-file-name (concat toncs-check-label ".json") output)
        (insert
         (json-encode
          `((check . ,toncs-check-label) (emacs_version . ,emacs-version)
            (failures . ,fatal) (upstream_warnings . ,upstream)
            (diagnostics .
                         ,(vconcat
                           (mapcar
                            (lambda (entry)
                              `((severity . ,(symbol-name (plist-get entry :severity)))
                                (origin . ,(symbol-name (plist-get entry :origin)))
                                (file . ,(plist-get entry :file))
                                (message . ,(plist-get entry :message))))
                            (reverse toncs-check-diagnostics)))))))))
    (when summary
      (with-temp-buffer
        (insert (format "### Warning check: %s\n\n" toncs-check-label)
                (format "Failures: **%d** · Upstream warnings (non-fatal): **%d**\n\n"
                        fatal upstream))
        (when toncs-check-diagnostics
          (insert "<details><summary>Diagnostics</summary>\n\n````text\n"
                  (string-replace "````" "` ` ` `" (toncs-check-diagnostic-text))
                  "\n````\n</details>\n\n"))
        (write-region (point-min) (point-max) summary t 'silent)))
    (message "Warning check: %d failure(s), %d upstream warning(s)" fatal upstream)))

(defun toncs-check-exit ()
  "Report diagnostics and fail for configuration/unknown warnings or any error."
  (toncs-check-write-reports)
  (when noninteractive
    (princ (concat (toncs-check-annotation-text) "\n")))
  (kill-emacs (if (cl-some #'toncs-check-fatal-p toncs-check-diagnostics) 1 0)))

(provide 'warning-monitor)
;;; warning-monitor.el ends here
