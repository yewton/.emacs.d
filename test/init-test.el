;;; init-test.el -*- lexical-binding: t -*-

(load (expand-file-name "test-helper" (file-name-directory load-file-name)))

(ert-deftest locale-test ()
  (should (string-equal current-language-environment "Japanese")))
