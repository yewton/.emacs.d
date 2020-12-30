(let ((base-directory (locate-dominating-file load-file-name "init.el")))
  (add-to-list 'load-path base-directory)
  (load "toncs-bootstrap")
  (load "early-init"))

(toncs-init)

(require 'toncs-el-get)

(toncs-el-get-init)
(el-get 'sync)

(require 'toncs-config)

(toncs-config-install)

(ert-deftest locale-test ()
  (should (string-equal current-language-environment "Japanese")))
