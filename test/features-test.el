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

(ert-deftest feature-dumb-jump-lazy-load-test ()
  (should-not toncs-test--dumb-jump-loaded-before-tests))

(ert-deftest feature-dumb-jump-xref-backend-test ()
  (require 'xref)
  (should (memq #'dumb-jump-xref-activate xref-backend-functions)))

(ert-deftest feature-dumb-jump-configure-test ()
  (require 'dumb-jump)
  (should (eq dumb-jump-force-searcher 'rg)))
