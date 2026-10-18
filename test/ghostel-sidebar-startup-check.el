;;; ghostel-sidebar-startup-check.el -*- lexical-binding: t -*-

;;; Also run by make test: emacs --batch --load test/ghostel-sidebar-startup-check.el

(load (expand-file-name "test-helper.el" (file-name-directory load-file-name)))
(require 'ert)
(defvar toncs-test-terminal-first nil)

(ert-deftest toncs-ghostel-sidebar-fresh-startup ()
  (should-not (featurep 'ghostel))
  (should (autoloadp (symbol-function 'toncs-ghostel-sidebar)))
  (let ((loads 0)
        (hook nil))
    (setq hook (lambda (file)
                 (when (member '(provide . toncs-config-ghostel)
                               (cdr (assoc file load-history)))
                   (setq loads (1+ loads)))))
    (add-hook 'after-load-functions hook)
    (unwind-protect
        (progn
          (when toncs-test-terminal-first
            (let ((parameters (copy-tree window-persistent-parameters)))
              (require 'ghostel)
              (should-not (featurep 'toncs-ghostel-sidebar))
              (with-temp-buffer
                (toncs-ghostel-register-terminal)
                (should (memq (current-buffer) toncs-ghostel-terminal-order)))
              (should-not (featurep 'toncs-ghostel-sidebar))
              (should (equal parameters window-persistent-parameters))))
          (let ((parameters (cl-remove-if
                             (lambda (entry)
                               (memq (car entry) '(window-side window-slot)))
                             (copy-tree window-persistent-parameters))))
            (call-interactively #'toncs-ghostel-sidebar)
            (should (equal parameters
                           (cl-remove-if (lambda (entry)
                                           (memq (car entry) '(window-side window-slot)))
                                         window-persistent-parameters))))
          (should (= loads 1))
          (should (featurep 'ghostel))
          (should (memq #'toncs-ghostel-register-terminal ghostel-mode-hook))
          (should consult-ghostel-mode)
          (should (eq major-mode 'toncs-ghostel-sidebar-mode)))
      (remove-hook 'after-load-functions hook)
      (when-let* ((buffer (get-buffer "*Ghostel Sidebar*")))
        (kill-buffer buffer)))))
(ert-run-tests-batch-and-exit)
