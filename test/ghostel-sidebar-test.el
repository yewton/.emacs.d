;;; ghostel-sidebar-test.el -*- lexical-binding: t -*-

(load (expand-file-name "test-helper" (file-name-directory load-file-name)))
(defconst toncs-test-sidebar-command-available-at-startup
  (commandp 'toncs-ghostel-sidebar))
(require 'toncs-ghostel-sidebar)

(defmacro toncs-test-with-sidebar (&rest body)
  "Run BODY with isolated registered terminals and a sidebar."
  (declare (indent 0))
  `(let ((first (generate-new-buffer " *sidebar-first*"))
         (second (generate-new-buffer " *sidebar-second*"))
         (sidebar (generate-new-buffer " *sidebar-test*"))
         (toncs-ghostel-terminal-order nil))
     (unwind-protect
         (progn
           (with-current-buffer first
             (setq-local major-mode 'ghostel-mode ghostel-title "Working")
             (toncs-ghostel-register-terminal))
           (with-current-buffer second
             (setq-local major-mode 'ghostel-mode ghostel-title nil)
             (toncs-ghostel-register-terminal))
           (with-current-buffer sidebar
             (toncs-ghostel-sidebar-mode)
             (toncs-ghostel-sidebar--start)
             ,@body))
       (dolist (buffer (list sidebar first second))
         (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest toncs-ghostel-sidebar-command-available-test ()
  (should toncs-test-sidebar-command-available-at-startup))

(ert-deftest toncs-ghostel-sidebar-refresh-test ()
  (toncs-test-with-sidebar
    (should (string-match-p "Working" (buffer-string)))
    (should (string-match-p "タイトルなし" (buffer-string)))
    (goto-char (toncs-ghostel-sidebar--position second))
    (with-current-buffer second
      (rename-buffer " *renamed-sidebar*" t)
      (setq ghostel-title (concat (make-string 100 ?x) "END\nTITLE")))
    (toncs-ghostel-sidebar-refresh)
    (should (eq (toncs-ghostel-sidebar--item) second))
    (should (equal (toncs-ghostel-sidebar--buffers) (list first second)))
    (should (string-match-p "END TITLE" (buffer-string)))
    (kill-buffer second)
    (toncs-ghostel-sidebar-refresh)
    (should-not (memq second toncs-ghostel-terminal-order))
    (should (eq (toncs-ghostel-sidebar--item) first))
    (toncs-ghostel-sidebar-mode)
    (kill-buffer first)
    (toncs-ghostel-sidebar-refresh)
    (should (string-match-p "ありません" (buffer-string)))))

(ert-deftest toncs-ghostel-sidebar-reject-wrong-mode-test ()
  (with-temp-buffer
    (insert "Keep this content")
    (goto-char 5)
    (dolist (command '(toncs-ghostel-sidebar-refresh toncs-ghostel-sidebar-update
                       toncs-ghostel-sidebar-next
                       toncs-ghostel-sidebar-previous toncs-ghostel-sidebar-select
                       toncs-ghostel-sidebar-quit))
      (should-error (funcall command) :type 'user-error)
      (should-error (call-interactively command) :type 'user-error)
      (should (equal (buffer-string) "Keep this content"))
      (should (= (point) 5)))
    (should-error (toncs-ghostel-sidebar-click nil) :type 'user-error)))

(ert-deftest toncs-ghostel-sidebar-navigation-test ()
  (toncs-test-with-sidebar
    (goto-char (point-max))
    (toncs-ghostel-sidebar-next)
    (should (eq (toncs-ghostel-sidebar--item) first))
    (goto-char (point-max))
    (toncs-ghostel-sidebar-previous)
    (should (eq (toncs-ghostel-sidebar--item) second))
    (let ((current-prefix-arg 2))
      (call-interactively #'toncs-ghostel-sidebar-previous))
    (should (eq (toncs-ghostel-sidebar--item) second))))

(ert-deftest toncs-ghostel-sidebar-timer-lifecycle-test ()
  (toncs-test-with-sidebar
    (let ((timer toncs-ghostel-sidebar--timer)
          (visible nil)
          (updates 0))
      (cl-letf (((symbol-function 'get-buffer-window)
                 (lambda (buffer frames)
                   (should (eq buffer sidebar))
                   (should (memq frames '(t visible)))
                   (if (eq frames t) t visible)))
                ((symbol-function 'toncs-ghostel-sidebar-refresh)
                 (lambda (&rest _) (setq updates (1+ updates)))))
        (dotimes (_ 2) (toncs-ghostel-sidebar--tick sidebar))
        (should (= updates 0))
        (should (eq timer toncs-ghostel-sidebar--timer))
        (setq visible t)
        (with-temp-buffer
          (toncs-ghostel-sidebar--tick sidebar)
          (should-not (local-variable-p 'toncs-ghostel-sidebar--timer)))
        (should (= updates 1)))
      (toncs-ghostel-sidebar-mode)
      (should-not (memq timer timer-list))
      (should-not toncs-ghostel-sidebar--timer)
      (toncs-ghostel-sidebar-update)
      (should (timerp toncs-ghostel-sidebar--timer))
      (setq timer toncs-ghostel-sidebar--timer)
      (fundamental-mode)
      (should-not (memq timer timer-list))
      (should-not toncs-ghostel-sidebar--timer))))

(ert-deftest toncs-ghostel-sidebar-timer-error-test ()
  (save-window-excursion
    (toncs-test-with-sidebar
      (display-buffer-in-side-window sidebar '((side . right) (slot . 1)))
      (let ((timer toncs-ghostel-sidebar--timer))
        (cl-letf (((symbol-function 'toncs-ghostel-sidebar-refresh)
                   (lambda (&rest _) (error "Injected refresh error"))))
          (toncs-ghostel-sidebar--tick sidebar))
        (should-not (memq timer timer-list))
        (should-not toncs-ghostel-sidebar--timer)
        (toncs-ghostel-sidebar-refresh t)
        (should-not toncs-ghostel-sidebar--timer)
        (toncs-ghostel-sidebar-update)
        (should (timerp toncs-ghostel-sidebar--timer))
        (setq timer toncs-ghostel-sidebar--timer)
        (kill-buffer sidebar)
        (should-not (memq timer timer-list))))))

(ert-deftest toncs-ghostel-sidebar-registration-and-discovery-test ()
  (toncs-test-with-sidebar
    (let ((toncs-ghostel-terminal-order nil))
      (with-current-buffer second (toncs-ghostel-register-terminal))
      (with-current-buffer first (toncs-ghostel-register-terminal))
      (dotimes (_ 2) (toncs-ghostel-sidebar-refresh))
      (should (equal (toncs-ghostel-sidebar--buffers) (list second first)))
      (with-current-buffer second
        (rename-buffer " *zzz-sidebar*" t)
        (toncs-ghostel-register-terminal)
        (fundamental-mode))
      (should (equal toncs-ghostel-terminal-order (list second first)))
      (toncs-ghostel-sidebar-refresh)
      (should (equal (toncs-ghostel-sidebar--buffers) (list first)))
      (with-current-buffer second
        (setq major-mode 'ghostel-mode)
        (toncs-ghostel-register-terminal))
      (toncs-ghostel-sidebar-refresh)
      (should (equal (toncs-ghostel-sidebar--buffers) (list second first)))
      (kill-buffer first)
      (toncs-ghostel-sidebar-refresh)
      (should (equal toncs-ghostel-terminal-order (list second))))))

(ert-deftest toncs-ghostel-sidebar-selection-and-click-test ()
  (save-window-excursion
    (toncs-test-with-sidebar
      (let* ((origin (selected-window))
             (window (display-buffer-in-side-window
                      sidebar '((side . right) (slot . 1)))))
        (set-window-parameter window 'toncs-ghostel-sidebar-target origin)
        (set-window-buffer origin first)
        ;; The selected buffer is a terminal when a sidebar item is clicked.
        (select-window origin)
        (let ((event (list 'mouse-1
                           (list window (with-current-buffer sidebar
                                          (toncs-ghostel-sidebar--position second))
                                 '(0 . 0) 0))))
          (toncs-ghostel-sidebar-click event))
        (should (eq (selected-window) origin))
        (should (eq (window-buffer origin) second))
        (should (eq (window-buffer window) sidebar))
        (select-window window)
        (with-current-buffer sidebar
          (set-window-parameter window 'toncs-ghostel-sidebar-target window)
          (goto-char (toncs-ghostel-sidebar--position first))
          (toncs-ghostel-sidebar-select))
        (should (eq (window-buffer origin) first))
        (select-window window)
        (with-current-buffer sidebar
          (kill-buffer first)
          (goto-char (toncs-ghostel-sidebar--position first))
          (should-error (toncs-ghostel-sidebar-select) :type 'user-error))))))

(ert-deftest toncs-ghostel-sidebar-normal-window-test ()
  (save-window-excursion
    (toncs-test-with-sidebar
      (rename-buffer "*Ghostel Sidebar*")
      (let ((normal (selected-window))
            (other (split-window-below)))
        (set-window-buffer normal sidebar)
        (with-current-buffer sidebar (toncs-ghostel-sidebar--stop))
        (select-window other)
        (toncs-ghostel-sidebar)
        (should (eq (selected-window) normal))
        (should (timerp toncs-ghostel-sidebar--timer))
        (should (= (length (get-buffer-window-list sidebar)) 1))
        ;; Even an origin pointing back to the list must use another window.
        (set-window-parameter normal 'toncs-ghostel-sidebar-target normal)
        (goto-char (toncs-ghostel-sidebar--position first))
        (toncs-ghostel-sidebar-select)
        (should (eq (selected-window) other))
        (should (eq (window-buffer other) first))
        (should (eq (window-buffer normal) sidebar))))))

(ert-deftest toncs-ghostel-sidebar-opening-failure-test ()
  (save-window-excursion
    (dolist (failure '(initialization display after-display after-display-quit))
      (let* ((origin (selected-window))
             (origin-buffer (window-buffer origin))
             (before-count (length (window-list)))
             (original-mode (symbol-function 'toncs-ghostel-sidebar-mode))
             (original-display (symbol-function 'display-buffer-in-side-window)))
        (unwind-protect
            (cl-letf (((symbol-function 'toncs-ghostel-sidebar-mode)
                       (lambda ()
                         (funcall original-mode)
                         (should-not toncs-ghostel-sidebar--timer)
                         (when (eq failure 'initialization)
                           (error "Injected initialization error"))))
                      ((symbol-function 'display-buffer-in-side-window)
                       (lambda (&rest args)
                         (unless (eq failure 'display)
                           (let ((window (apply original-display args)))
                             (pcase failure
                               ('after-display (error "Injected display error"))
                               ('after-display-quit (signal 'quit nil)))
                             window)))))
              (should (eq (condition-case nil
                              (progn (toncs-ghostel-sidebar) 'no-error)
                            (quit 'quit) (error 'error))
                          (if (eq failure 'after-display-quit) 'quit 'error)))
              (should (eq (selected-window) origin))
              (should (eq (window-buffer origin) origin-buffer))
              (should (= (length (window-list)) before-count))
              (should-not (get-buffer-window "*Ghostel Sidebar*"))
              (should-not (window-dedicated-p origin))
              (with-current-buffer "*Ghostel Sidebar*"
                (should-not toncs-ghostel-sidebar--timer)))
          (when-let* ((buffer (get-buffer "*Ghostel Sidebar*")))
            (kill-buffer buffer)))))))

(ert-deftest toncs-ghostel-sidebar-toggle-and-close-test ()
  (save-window-excursion
    (let ((renders 0)
          (refresh (symbol-function 'toncs-ghostel-sidebar-refresh)))
      (unwind-protect
          (cl-letf (((symbol-function 'toncs-ghostel-sidebar-refresh)
                     (lambda (&rest args)
                       (setq renders (1+ renders))
                       (apply refresh args))))
            (toncs-ghostel-sidebar)
            (should (= renders 1))
            (let ((timer toncs-ghostel-sidebar--timer)
                  (sidebar (current-buffer)))
              (toncs-ghostel-sidebar-quit)
              (should-not (get-buffer-window sidebar))
              (should-not (memq timer timer-list))
              (toncs-ghostel-sidebar)
              (should (= renders 2))
              (setq timer toncs-ghostel-sidebar--timer)
              (should (timerp timer))
              (toncs-ghostel-sidebar)
              (should-not (get-buffer-window sidebar))
              (should-not (memq timer timer-list))))
        (when-let* ((buffer (get-buffer "*Ghostel Sidebar*")))
          (kill-buffer buffer))))))

(ert-deftest toncs-ghostel-sidebar-dead-record-test ()
  (save-window-excursion
    (toncs-test-with-sidebar
      (display-buffer-in-side-window sidebar '((side . right) (slot . 1)))
      (let ((timer toncs-ghostel-sidebar--timer))
        (with-current-buffer second
          (let ((kill-buffer-hook nil)) (kill-buffer second)))
        (should (memq second toncs-ghostel-terminal-order))
        (toncs-ghostel-sidebar--tick sidebar)
        (should (equal toncs-ghostel-terminal-order (list first)))
        (should (equal (toncs-ghostel-sidebar--buffers) (list first)))
        (should (eq timer toncs-ghostel-sidebar--timer))))))

(ert-deftest toncs-ghostel-sidebar-stale-selection-keeps-stopped-test ()
  (toncs-test-with-sidebar
    (goto-char (toncs-ghostel-sidebar--position second))
    (kill-buffer second)
    (toncs-ghostel-sidebar--stop)
    (should-error (toncs-ghostel-sidebar-select) :type 'user-error)
    (should-not toncs-ghostel-sidebar--timer)))

(ert-deftest toncs-ghostel-sidebar-panel-isolation-test ()
  (save-window-excursion
    (unwind-protect
        (let ((origin (selected-window)))
          (toncs-ghostel-sidebar)
          (let ((sidebar-window (selected-window)))
            (select-window origin)
            (let* ((buffer (get-buffer-create " *nonzero-panel*"))
                   (panel (display-buffer-in-side-window
                           buffer '((side . right) (slot . 3)))))
              (unwind-protect
                  (progn
                    (toncs-side-panel-right)
                    (should-not (window-live-p panel))
                    (should (window-live-p sidebar-window))
                    (select-window origin)
                    (delete-other-windows)
                    (should (window-live-p sidebar-window)))
                (kill-buffer buffer)))))
      (when-let* ((buffer (get-buffer "*Ghostel Sidebar*")))
        (kill-buffer buffer)))))


(ert-deftest toncs-ghostel-sidebar-external-window-close-test ()
  (save-window-excursion
    (toncs-test-with-sidebar
      (let ((window (display-buffer-in-side-window
                     sidebar '((side . right) (slot . 1))))
            (timer toncs-ghostel-sidebar--timer))
        (delete-window window)
        (toncs-ghostel-sidebar--tick sidebar)
        (should-not toncs-ghostel-sidebar--timer)
        (should-not (memq timer timer-list))))))

(ert-deftest toncs-ghostel-sidebar-unchanged-refresh-skips-positions-test ()
  (toncs-test-with-sidebar
    (let ((calls 0)
          (item (symbol-function 'toncs-ghostel-sidebar--item)))
      (cl-letf (((symbol-function 'toncs-ghostel-sidebar--item)
                 (lambda (&rest args)
                   (setq calls (1+ calls))
                   (apply item args))))
        (toncs-ghostel-sidebar-refresh)
        (should (= calls 0))
        (with-current-buffer first (setq ghostel-title "Changed"))
        (toncs-ghostel-sidebar-refresh)
        (should (> calls 0))))))


(ert-deftest toncs-ghostel-sidebar-new-window-only-test ()
  (save-window-excursion
    (let* ((buffer (generate-new-buffer " *existing-panel*"))
           (origin (selected-window))
           (panel (display-buffer-in-side-window
                   buffer '((side . right) (slot . -1))))
           (dedicated buffer))
      (set-window-dedicated-p panel dedicated)
      (set-window-parameter panel 'no-delete-other-windows 'existing-setting)
      (unwind-protect
          (progn
            (select-window origin)
            (toncs-ghostel-sidebar)
            (let ((sidebar-window (selected-window)))
              (should-not (eq panel sidebar-window))
              (should (eq (window-buffer panel) buffer))
              (should (eq (window-dedicated-p panel) dedicated))
              (toncs-ghostel-sidebar-quit)
              (should-not (window-live-p sidebar-window)))
            (should (eq (window-buffer panel) buffer))
            (should (eq (window-dedicated-p panel) dedicated))
            (should (eq (window-parameter panel 'no-delete-other-windows)
                        'existing-setting))
            (cl-letf (((symbol-function 'split-window)
                       (lambda (&rest _) (error "Cannot create a window"))))
              (should-error (toncs-ghostel-sidebar)))
            (should (eq (window-buffer panel) buffer))
            (should-not (get-buffer-window "*Ghostel Sidebar*")))
        (when (window-live-p panel) (delete-window panel))
        (kill-buffer buffer)
        (when-let* ((sidebar (get-buffer "*Ghostel Sidebar*")))
          (kill-buffer sidebar))))))

(ert-deftest toncs-ghostel-sidebar-normal-window-settings-test ()
  (save-window-excursion
    (toncs-test-with-sidebar
      (let ((window (selected-window)))
        (set-window-buffer window sidebar)
        (set-window-parameter window 'no-delete-other-windows 'user-setting)
        (set-window-dedicated-p window 'side)
        (toncs-ghostel-sidebar-quit)
        (should (window-live-p window))
        (should (eq (window-parameter window 'no-delete-other-windows)
                    'user-setting))
        (should (eq (window-dedicated-p window) 'side))))))

(ert-deftest toncs-ghostel-sidebar-refresh-interval-test ()
  (toncs-test-with-sidebar
    (toncs-ghostel-sidebar--stop)
    (let ((toncs-ghostel-sidebar-refresh-interval 0.2)
          (scheduled nil))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (delay repeat function &rest args)
                   (setq scheduled (list delay repeat function args))
                   nil)))
        (toncs-ghostel-sidebar-update))
      (should (equal (cl-subseq scheduled 0 2) '(0.2 0.2)))
      (should (functionp (nth 2 scheduled)))
      (should-not (nth 3 scheduled)))
    (let ((toncs-ghostel-sidebar-refresh-interval 0))
      (should-error (toncs-ghostel-sidebar-update) :type 'user-error))))


(ert-deftest toncs-ghostel-sidebar-fixed-slot-test ()
  (save-window-excursion
    (let* ((origin (selected-window))
           (buffer (generate-new-buffer " *negative-slot-panel*"))
           (panel (display-buffer-in-side-window
                   buffer '((side . right) (slot . -1)))))
      (unwind-protect
          (progn
            (toncs-ghostel-sidebar)
            (let ((sidebar (current-buffer))
                  (window (selected-window)))
              (should (= (window-parameter window 'window-slot) 1))
              (select-window origin)
              (toncs-side-panel-right)
              (should-not (window-live-p panel))
              (toncs-side-panel-right)
              (should (= (window-parameter nil 'window-slot) 0))
              (should (eq (window-buffer window) sidebar))
              (should (= (window-parameter window 'window-slot) 1))))
        (kill-buffer buffer)
        (when-let* ((sidebar (get-buffer "*Ghostel Sidebar*")))
          (kill-buffer sidebar))))))

(ert-deftest toncs-ghostel-sidebar-slot-collision-test ()
  (save-window-excursion
    (let* ((buffer (generate-new-buffer " *slot-one-panel*"))
           (panel (display-buffer-in-side-window
                   buffer '((side . right) (slot . 1)))))
      (unwind-protect
          (progn
            (should-error (toncs-ghostel-sidebar) :type 'user-error)
            (should (eq (window-buffer panel) buffer))
            (should (= (window-parameter panel 'window-slot) 1)))
        (kill-buffer buffer)
        (when-let* ((sidebar (get-buffer "*Ghostel Sidebar*")))
          (kill-buffer sidebar))))))

(ert-deftest toncs-ghostel-terminal-kill-cleanup-test ()
  (let ((toncs-ghostel-terminal-order nil)
        (buffer (generate-new-buffer " *registered-terminal*")))
    (unwind-protect
        (with-current-buffer buffer
          (toncs-ghostel-register-terminal)
          (fundamental-mode)
          (should (memq buffer toncs-ghostel-terminal-order))
          (kill-buffer buffer)
          (should-not toncs-ghostel-terminal-order))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest toncs-ghostel-sidebar-unregistered-terminal-test ()
  (toncs-test-with-sidebar
    (let ((unregistered (generate-new-buffer " *missed-terminal*"))
          (current (current-buffer))
          (predicate (symbol-function 'provided-mode-derived-p)))
      (unwind-protect
          (progn
            (put 'toncs-test-ghostel-child-mode 'derived-mode-parent 'ghostel-mode)
            (with-current-buffer unregistered
              (setq major-mode 'toncs-test-ghostel-child-mode)
              (setq-local ghostel-title "Missed by hook"))
            (cl-letf (((symbol-function 'provided-mode-derived-p)
                       (lambda (&rest args)
                         (should (eq (current-buffer) current))
                         (apply predicate args))))
              (toncs-ghostel-sidebar-refresh))
            (should (equal (toncs-ghostel-sidebar--buffers)
                           (list first second unregistered)))
            (should (string-match-p "Missed by hook" (buffer-string))))
        (kill-buffer unregistered)))))

(ert-deftest toncs-ghostel-sidebar-discovery-then-registration-order-test ()
  (toncs-test-with-sidebar
    (let ((discovered (generate-new-buffer " *discovered-terminal*"))
          (new (generate-new-buffer " *new-terminal*")))
      (unwind-protect
          (progn
            (with-current-buffer discovered
              (setq major-mode 'ghostel-mode))
            (toncs-ghostel-sidebar-refresh)
            (should (equal (toncs-ghostel-sidebar--buffers)
                           (list first second discovered)))
            (with-current-buffer new
              (setq major-mode 'ghostel-mode)
              (toncs-ghostel-register-terminal))
            (toncs-ghostel-sidebar-refresh)
            (should (equal (toncs-ghostel-sidebar--buffers)
                           (list first second discovered new)))
            (kill-buffer discovered)
            (should-not (memq discovered toncs-ghostel-terminal-order)))
        (when (buffer-live-p discovered) (kill-buffer discovered))
        (kill-buffer new)))))

(ert-deftest toncs-ghostel-sidebar-zero-movement-test ()
  (toncs-test-with-sidebar
    (dolist (position (list (point-max)
                           (+ 2 (toncs-ghostel-sidebar--position first))))
      (goto-char position)
      (dolist (command '(toncs-ghostel-sidebar-next toncs-ghostel-sidebar-previous))
        (let ((current-prefix-arg 0)) (call-interactively command))
        (should (= (point) position))))))

(ert-deftest toncs-ghostel-sidebar-invalid-interval-before-opening-test ()
  (let ((toncs-ghostel-sidebar-refresh-interval 0))
    (cl-letf (((symbol-function 'get-buffer-create)
               (lambda (&rest _) (ert-fail "Created a buffer before validation")))
              ((symbol-function 'toncs-ghostel-sidebar--create-window)
               (lambda (&rest _) (ert-fail "Created a window before validation"))))
      (should-error (toncs-ghostel-sidebar) :type 'user-error))))

(ert-deftest toncs-ghostel-sidebar-invalid-interval-preserves-timer-test ()
  (toncs-test-with-sidebar
    (let ((toncs-ghostel-sidebar-refresh-interval -1)
          (timer toncs-ghostel-sidebar--timer)
          (contents (buffer-string)))
      (with-current-buffer first (setq ghostel-title "Not yet rendered"))
      (should-error (toncs-ghostel-sidebar-update) :type 'user-error)
      (should (eq timer toncs-ghostel-sidebar--timer))
      (should (equal contents (buffer-string))))))

(ert-deftest toncs-ghostel-sidebar-close-with-invalid-interval-test ()
  (save-window-excursion
    (unwind-protect
        (progn
          (toncs-ghostel-sidebar)
          (let ((sidebar (current-buffer))
                (toncs-ghostel-sidebar-refresh-interval 0))
            (toncs-ghostel-sidebar)
            (should-not (get-buffer-window sidebar))))
      (when-let* ((sidebar (get-buffer "*Ghostel Sidebar*")))
        (kill-buffer sidebar)))))


(ert-deftest toncs-ghostel-sidebar-orphan-timer-test ()
  (toncs-test-with-sidebar
    (let ((timer toncs-ghostel-sidebar--timer))
      (let ((kill-buffer-hook nil)) (kill-buffer sidebar))
      (should (memq timer timer-list))
      (apply (timer--function timer) (timer--args timer))
      (should-not (memq timer timer-list)))))

(ert-deftest toncs-ghostel-sidebar-update-checks-mode-first-test ()
  (with-temp-buffer
    (let ((toncs-ghostel-sidebar-refresh-interval 0))
      (should (equal (should-error (toncs-ghostel-sidebar-update) :type 'user-error)
                     '(user-error "Ghostel サイドバーで実行してください"))))))

(ert-deftest toncs-ghostel-sidebar-stale-timer-test ()
  (toncs-test-with-sidebar
    (let ((old toncs-ghostel-sidebar--timer))
      (toncs-ghostel-sidebar-update)
      (let ((current toncs-ghostel-sidebar--timer))
        (apply (timer--function old) (timer--args old))
        (should (eq current toncs-ghostel-sidebar--timer))
        (should (memq current timer-list))))))
