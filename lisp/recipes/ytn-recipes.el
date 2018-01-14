;;; ytn-recipes.el --- Package recipes -*- lexical-binding: t -*-

;; Copyright (C) 2018 Yuto SASAKI
;; Author: Yuto SASAKI <yewton@gmail.com>

;;; Commentary:

;; Define package recipes(i.e. el-get-sources).

;;; Code:

(defvar el-get-sources)
(setq el-get-sources '((:name exec-path-from-shell :checkout "5e355fbc50913d1ffe48bf86df0bcecd8b369ffb")

                       (:name dash :checkout "528e5a51f1af668e3075f2beccd2b39785ccb2ba")
                       (:name s :checkout "5e9a6857d42015c67681616aa3519f599f97b8d8")
                       (:name f :checkout "de6d4d40ddc844eee643e92d47b9d6a63fbebb48")

                       (:name use-package :checkout "05a4033b830bf52c596ceedea10b2cbd91f85fdb")
                       (:name delight :checkout "05ef4d7d1a371884defcb47e881904f2a25a40b7")

                       (:name swiper :checkout "6da6d70b1dbc4df41e9745eadf8b2697b292d968")

                       (:name ivy-xref :checkout "aa97103ea8ce6ab8891e34deff7d43aa83fe36dd")

                       (:name winum :branch "2.0.0")

                       (:name crux :checkout "3e07035660f953cb08847362378267f5859bbe69")

                       (:name packed :checkout "94ea12b9d44bfa42c28d0548199f2fcd19e4aa6a")
                       (:name auto-compile
                        ;; cf. https://github.com/dimitri/el-get/pull/2593
                        :depends (packed)
                        :checkout "8d117868a0a091389d528428136e60f951e9c550")

                       (:name epl :checkout "83797835f729f39b80acba4c7e83d73a2e410e26")
                       (:name pkg-info :checkout "76ba7415480687d05a4353b27fea2ae02b8d9d61")
                       (:name flycheck :checkout "6bc54f00666d14197cb8685b42dbd49e19c82ec8")

                       (:name pos-tip
                        ;; cf. https://github.com/dimitri/el-get/pull/2597
                        :type github
                        :pkgname "pitkali/pos-tip"
                        :checkout "051e08fec5cf30b7574bdf439f79fef7d42d689d")
                       (:name flycheck-pos-tip :checkout "7b94c0c1e3185a4350dc39885add3927037ebad8")

                       (:name emacs-async :checkout "324549ba1dcf610c3766c272f86bae9d2c49fc70")
                       (:name ghub :checkout "1395d56496c1581dda0c70a091500e2b947b8d35")
                       (:name with-editor :checkout "04d59d68dab58a7cf3034c84d8ba0553b78ae30c")
                       (:name magit-popup :checkout "3f23e81eb0267d6578d8f1733c5e42699b0229a1")
                       (:name magit :branch "2.11.0")

                       (:name smex :checkout "55aaebe3d793c2c990b39a302eb26c184281c42c")
                       
                       (:name projectile :checkout "293849974df45b60bad572bfdc5166575fbbf0a5")
                       (:name counsel-projectile :checkout "4d78ae8c90e8ebb903b8e70442989a69e46ff069")

                       (:name hydra :checkout "1deed8a00e6936903cace1dac123364b6c0cde90")

                       (:name which-key :checkout "1219622b756f149efe4b44c625f2140c5229f936")

                       (:name golden-ratio :checkout "72b028808b41d23fa3f7e8c0d23d2c475e7b46ae")

                       (:name buffer-move :checkout "cb517ecf8409b5fdcda472d7190c6021f0c49751")

                       (:name org-mode :branch "release_9.1.6"
                        ;; cf. https://github.com/dimitri/el-get/pull/2590
                        :url "https://code.orgmode.org/bzg/org-mode.git")))

(provide 'ytn-recipes)
;;; ytn-recipes.el ends here
