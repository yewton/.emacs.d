;;; ytn-recipes.el --- Package recipes -*- lexical-binding: t -*-

;; Copyright (C) 2018 Yuto SASAKI
;; Author: Yuto SASAKI <yewton@gmail.com>

;;; Commentary:

;; Setup package install recipese (i.e. `el-get-sources').

;;; Code:

(defvar el-get-sources)

;;;###autoload
(defun ytn-recipes-setup ()
  "Setup package install recipese (i.e. `el-get-sources')."
  (setq el-get-sources '((:name el-get)
                         (:name exec-path-from-shell :checkout "5e355fbc50913d1ffe48bf86df0bcecd8b369ffb")

                         (:name package :after
                                ;; To avoid long initialization phase of melpa repo
                                ;; and possible issues due to marmalade repo.
                                ;; This recipe is required by type `elpa' package.
                                ;; cf. https://github.com/nicferrier/elmarmalade/issues/138
                                ;;     https://github.com/dimitri/el-get/blob/2d9068f7bc2aa0b2ad2e9cbb2022e72ac737eaa7/recipes/package.rcp#L33-L38
                                (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/"))))

                         (:name dash :checkout "528e5a51f1af668e3075f2beccd2b39785ccb2ba")
                         (:name s :checkout "5e9a6857d42015c67681616aa3519f599f97b8d8")
                         (:name f :checkout "de6d4d40ddc844eee643e92d47b9d6a63fbebb48")
                         (:name ht :checkout "64af52688eb09eb42b7228a4e8e40d4a81cd983b")

                         (:name no-littering :checkout "c6de27af80edadc3cc09fe8a6832058d00ad570c")

                         (:name delight :checkout "05ef4d7d1a371884defcb47e881904f2a25a40b7")
                         (:name use-package :checkout "05a4033b830bf52c596ceedea10b2cbd91f85fdb")

                         (:name diredfl :shallow t :branch "0.4")

                         (:name nlinum :repo ("gnu" . "https://elpa.gnu.org/packages/"))

                         (:name hydra :checkout "1deed8a00e6936903cace1dac123364b6c0cde90")

                         (:name smex :checkout "55aaebe3d793c2c990b39a302eb26c184281c42c")
                         (:name swiper
                                :shallow t
                                :branch "0.10.0"
                                :build (("makeinfo" "-o" "doc/ivy.info" "doc/ivy.texi")))

                         (:name ivy-xref
                                :checkout "aa97103ea8ce6ab8891e34deff7d43aa83fe36dd"
                                :compile nil)

                         (:name wgrep :checkout "1cdd7c136f1e7565bb13d2df69be3dc77b83698d")
                         (:name ag :checkout "d00aa65ec2da6944f1ed81da440ad7a9024cfbf0")
                         (:name rg :checkout "42603a9279b13427f7049c2768bcc6213094ef18")

                         (:name winum :shallow t :branch "2.0.0" :compile nil)

                         (:name crux :checkout "3e07035660f953cb08847362378267f5859bbe69")

                         (:name packed :checkout "94ea12b9d44bfa42c28d0548199f2fcd19e4aa6a")
                         (:name auto-compile
                                :checkout "8d117868a0a091389d528428136e60f951e9c550"
                                ;; cf. https://github.com/dimitri/el-get/pull/2593
                                :depends (packed)
                                :compile nil)

                         (:name epl :checkout "83797835f729f39b80acba4c7e83d73a2e410e26")
                         (:name pkg-info :checkout "76ba7415480687d05a4353b27fea2ae02b8d9d61")
                         (:name flycheck :shallow t :branch "31" :compile nil)

                         (:name pos-tip
                                ;; cf. https://github.com/dimitri/el-get/pull/2597
                                :type github
                                :pkgname "pitkali/pos-tip"
                                :checkout "051e08fec5cf30b7574bdf439f79fef7d42d689d")
                         (:name flycheck-pos-tip
                                :checkout "7b94c0c1e3185a4350dc39885add3927037ebad8"
                                :compile nil)

                         (:name emacs-async :checkout "324549ba1dcf610c3766c272f86bae9d2c49fc70")
                         (:name ghub :checkout "1395d56496c1581dda0c70a091500e2b947b8d35")
                         (:name with-editor :checkout "04d59d68dab58a7cf3034c84d8ba0553b78ae30c")
                         (:name magit-popup :checkout "3f23e81eb0267d6578d8f1733c5e42699b0229a1")
                         (:name magit :shallow t :branch "2.13.0")
                         (:name git-modes :shallow t :branch "1.2.6")

                         (:name diff-hl :checkout "bec9889de7bf48d28826039880cec9bfad24a628")

                         (:name projectile
                                :checkout "9482d69e5ca6549ec7baea58f86c1445b544563b"
                                :compile nil)
                         (:name counsel-projectile
                                :checkout "7607fb8bb4eb7fbe0ec20f9644b6bbaa5c363330"
                                :compile nil)

                         (:name which-key :shallow t :branch "v3.1.0" :compile nil)

                         (:name zoom :checkout "d4f417b24fe022b6512a1a0ffebf898044393250")

                         (:name buffer-move :checkout "cb517ecf8409b5fdcda472d7190c6021f0c49751")

                         (:name rainbow-identifiers :shallow t :branch "0.2.2")
                         (:name rainbow-delimiters :shallow t :branch "2.1.3")

                         (:name company-mode :shallow t :branch "0.9.4")
                         (:name company-quickhelp :checkout "1580b10")

                         (:name powerline :shallow t :branch "2.4")
                         (:name spaceline :checkout "ca20430271f80d46892d62cb7624b4dee6cafe72" :compile nil)

                         (:name avy :checkout "34d3531a3cd35ae6b9e8dbc37343c027504fdcb0")
                         (:name ace-window :checkout "c0fbdd51be8fdd43fd7a89140ebd4b4c311d45fc")
                         (:name pfuture :checkout "d55a764b5beee803e489974be2408dbe236fc482")
                         (:name treemacs :checkout "692f5be4ab6a4868978dcb2639334f78a267cfd2")

                         (:name ace-link :depends (avy)
                                :checkout "43d224546a2a557857294a8e3d13c4fe63508e03")

                         (:name browse-at-remote :checkout "31dcf77d7c89a12f230e2b2332585db2c44530ef")

                         (:name ddskk :checkout "16d156562aef72f02ced58087335ed72d520d878")

                         (:name org-mode :checkout "ffa5f3c5198c87454a9957f81380d52d2bbb96a0")

                         ;; (:name org-mode
                         ;;        :branch "release_9.1.6"
                         ;;        :shallow t
                         ;;        ;; cf. https://github.com/dimitri/el-get/pull/2590
                         ;;        :url "https://code.orgmode.org/bzg/org-mode.git")

                         (:name info-plus :type emacsmirror
                                :checkout "ac3aa3905e8ad2a99a0225f2e6a83a54541cf197")
                         (:name help-plus :type emacsmirror
                                :checkout "a702d717dda84dd45c9dcc677bdf580b8275d5a7")
                         (:name help-fns-plus :type emacsmirror
                                :checkout "7213ef84d0345e2625f28e2b3134f8d98489391f")

                         (:name loop :checkout "e22807f83a0890dc8a904c51ee0742c34efccc6c")
                         (:name elisp-refs :checkout "eee751a6120f925cdffcfbb6a4545e599b953e94")
                         (:name shut-up :checkout "a4fd18f37e20ae991c0dbba821b2c8e6f1679c39")
                         (:name helpful :checkout "5eb8368c429833309abaa5c42e170e6fad84a19f")

                         (:name emojify :checkout "001c3adcc521223f6b53a2243635528b2cb7f7e8")

                         (:name yasnippet :checkout "203059a95e320b031ac0d2cabe9c1de68604baec")
                         (:name yasnippet-snippets :checkout "f76efc2054ac41434544de96ed0a09a455b6c040")

                         (:name avy-menu :checkout "990cc94d708c923f761be083b3a57f6f844566c8")
                         (:name ace-popup-menu :checkout "7b8ad628a058d32c420f7615927a34a5d51a7ad3")

                         (:name highlight-thing :checkout "4eadd178175772fb04ae50e1199d797a6375ad4d")

                         (:name visual-regexp :shallow t :branch "v1.1.1")

                         (:name edit-indirect :checkout "032ac0ec690d4999d564fd882588c7a197efe8dd")
                         (:name markdown-mode :checkout "1c343f5ce4213e6a6e9562c4ab621a1f8e6c31c5" :prepare nil)

                         (:name plantuml-mode :pkgname "yewton/plantuml-mode" :branch "experimental")

                         (:name yaml-mode :checkout "3fc5a33760b0bbb6e67adbce48ab3dc4ae34b847")

                         (:name hcl-mode :checkout "0f2c5e")
                         (:name terraform-mode :checkout "6973d1")
                         (:name company-terraform :pkgname "rafalcieslak/emacs-company-terraform" :type github :checkout "74dad2")

                         (:name scala-mode :shallow t :branch "v1.0.0")
                         (:name sbt-mode :shallow t :branch "v1.0.1")

                         (:name groovy-emacs-mode :checkout "0aea74def58791b2343a8f0139c2f2a6a0941877")

                         (:name leuven-theme :checkout "77dcc0bd277541ce90438e9ea9fc3f3fe5d90e22"))))

(provide 'ytn-recipes)
;;; ytn-recipes.el ends here
