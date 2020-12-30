(load (locate-user-emacs-file "toncs-bootstrap.el"))
(load (locate-user-emacs-file "early-init.el"))

(toncs-init)

(require 'toncs-el-get)

(toncs-el-get-init)
(el-get 'sync)

(require 'toncs-config)

(toncs-config-install)

(ert-deftest locale-test ()
  (should (string-equal current-language-environment "Japanese")))
