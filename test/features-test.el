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

(dolist (prepare-function toncs-config-prepare-functions)
  (let* ((feature (toncs-test--prepare-function-feature prepare-function))
         (test-name (intern (format "feature-%s-test" feature))))
    (eval
     `(ert-deftest ,test-name ()
        (require ',feature)
        (should (featurep ',feature)))
     t)))
