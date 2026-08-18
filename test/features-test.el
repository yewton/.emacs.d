;;; features-test.el -*- lexical-binding: t -*-

(load (expand-file-name "test-helper" (file-name-directory load-file-name)))

(require 'subr-x)

(defun toncs-test--prepare-function-feature (prepare-function-symbol)
  "Derive the feature symbol registered via `toncs-config-prepare'
from PREPARE-FUNCTION-SYMBOL, a `toncs-config-FEATURE-prepare' symbol."
  (intern
   (string-remove-suffix
    "-prepare"
    (string-remove-prefix "toncs-config-" (symbol-name prepare-function-symbol)))))

(defconst toncs-test--dumb-jump-loaded-before-tests (featurep 'dumb-jump)
  "起動直後（テスト実行前）に feature `dumb-jump' がロード済みだったかどうか。")

(dolist (prepare-function toncs-config-prepare-functions)
  (let* ((feature (toncs-test--prepare-function-feature prepare-function))
         (test-name (intern (format "feature-%s-test" feature))))
    (eval
     `(ert-deftest ,test-name ()
        (require ',feature)
        (should (featurep ',feature)))
     t)))

(dolist (entry toncs-test-configure-function-snapshot)
  (let* ((feature (nth 0 entry))
         (definition (nth 2 entry))
         (test-name (intern (format "feature-%s-configure-not-shadowed-test" feature))))
    (eval
     `(ert-deftest ,test-name ()
        "`toncs-config-prepare' が autoload 宣言した configure 関数が、
別ファイルの `toncs-config-configure' 呼び出しによって上書きされていないか
確認する。上書きされると、専用ファイル lisp/toncs-config-FEATURE.org の
設定が一切読み込まれなくなる（コミット 88e0c8d のバグと同種）。"
        (should (autoloadp ',definition)))
     t)))

(ert-deftest feature-dumb-jump-lazy-load-test ()
  (should-not toncs-test--dumb-jump-loaded-before-tests))

(ert-deftest feature-dumb-jump-xref-backend-test ()
  (require 'xref)
  (should (memq #'dumb-jump-xref-activate xref-backend-functions)))

(ert-deftest feature-dumb-jump-configure-test ()
  (require 'dumb-jump)
  (should (eq dumb-jump-force-searcher 'rg)))
