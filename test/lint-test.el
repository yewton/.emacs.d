;;; lint-test.el -*- lexical-binding: t -*-

(load (expand-file-name "test-helper" (file-name-directory load-file-name)))

(ert-deftest lexical-binding-cookie-test ()
  "no-byte-compile なファイルに lexical-binding cookie があることを確認する。

`lisp/*.el' はバイトコンパイル（`make lisp'）を通して
`ERROR_ON_WARN=t' がこの種の警告を検知するが、no-byte-compile な
ファイルはコンパイルされないため検知網から漏れる。"
  (dolist (file '("toncs-bootstrap.el" "early-init.el" "init.el"
                  "etc/borg/config.el"))
    (let ((path (expand-file-name file user-emacs-directory)))
      (should (file-exists-p path))
      (with-temp-buffer
        (insert-file-contents path nil 0 200)
        (should (string-match-p "lexical-binding: *t" (buffer-string)))))))
