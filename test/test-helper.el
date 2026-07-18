;;; test-helper.el -*- lexical-binding: t -*-

(defvar toncs-test-helper-loaded nil
  "Non-nil once the shared test startup has already run.")

(unless toncs-test-helper-loaded
  (let ((base-directory (locate-dominating-file load-file-name "init.el")))
    (add-to-list 'load-path base-directory)
    (load "toncs-bootstrap")
    (load "early-init"))

  (require 'toncs-stdlib)
  (toncs-init)

  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize)

  (require 'toncs-config)

  (toncs-config-install)

  (setq toncs-test-helper-loaded t))
