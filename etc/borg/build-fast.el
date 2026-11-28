;;; build-fast.el --- borg.mk の build-fast で読み込む設定 -*- lexical-binding: t; -*-

;; build-fast は up-to-date な .elc を作り直さずに全 drone をビルドする。
;;
;; drone はそれぞれ別の Emacs プロセスでビルドする。1 プロセスで続けて
;; ビルドすると、先の drone が残した状態 (advice・ロード済みのライブラリ) が
;; 後の drone のコンパイル結果に入り込む。実例として、ddskk が
;; `newline-and-indent' に付ける advice と、ob-restclient のビルド中に
;; ソースのままロードされた org が重なると、org.elc に読み込めない
;; `#<subr newline-and-indent>' が埋め込まれ、(require 'org) が失敗した。
;;
;; ビルド順序とスキップ判定は `borg-batch-rebuild' に任せ、
;; drone ごとのビルド (`borg-build') だけを子プロセスに置き換える。

(require 'borg)

;; 親 (build-fast) と子 (build-fast/DRONE) の両方に効かせる設定
(setq borg-compile-function (lambda (file) (byte-recompile-file file nil 0)))
(advice-add 'borg-clean :around #'ignore)
(advice-add 'borg--remove-autoloads :around #'ignore)

(defvar toncs-borg-build-fast-make "make -f borg.mk"
  "1 drone をビルドするときに呼ぶ make コマンド。borg.mk が $(MAKE) を渡す。")

(defun toncs-borg-build-in-subprocess (drone &rest _)
  "DRONE を build-fast/DRONE ターゲットで別プロセスとしてビルドする。"
  (with-temp-buffer
    (let ((status (call-process-shell-command
                   (format "%s build-fast/%s" toncs-borg-build-fast-make
                           (shell-quote-argument drone))
                   nil t)))
      (unless (bobp)
        (message "%s" (string-trim-right (buffer-string))))
      (unless (eq status 0)
        (error "Building %s failed (exit status %s)" drone status)))))

(defun toncs-borg-build-fast ()
  "全 drone を 1 つずつ別プロセスでビルドし、最後に init ファイルをビルドする。"
  (advice-add 'borg-build :override #'toncs-borg-build-in-subprocess)
  (borg-batch-rebuild))

;;; build-fast.el ends here
