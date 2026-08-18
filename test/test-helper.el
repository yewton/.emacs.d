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
  (require 'subr-x)

  (defvar toncs-test-configure-function-snapshot
    (mapcar
     (lambda (prepare-function)
       (let* ((feature (intern (string-remove-suffix
                                 "-prepare"
                                 (string-remove-prefix "toncs-config-" (symbol-name prepare-function)))))
              (configure-function (intern (format "toncs-config-%s-configure" feature))))
         (list feature configure-function (symbol-function configure-function))))
     toncs-config-prepare-functions)
    "`toncs-config-install' 実行前、feature がまだ何も require されていない
時点で記録した (FEATURE CONFIGURE-FUNCTION DEFINITION) のリスト。
`toncs-config-prepare' の autoload 宣言が別の `toncs-config-configure' に
上書きされていないかは、feature ロード前のこのタイミングでしか観測できない。")

  (toncs-config-install)

  (setq toncs-test-helper-loaded t))
