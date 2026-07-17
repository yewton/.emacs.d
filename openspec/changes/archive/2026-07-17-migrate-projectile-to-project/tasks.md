# Tasks: Migrate projectile to built-in project.el

## 1. toncs-config.org の編集

- [x] 1.1 `* projectile` セクションを `* project` セクションに置き換える
      （`toncs-projectile-ignored-project-function`・キャッシュ・delight・
      `C-c p` バインド・`consult-project-function` 上書きを削除し、
      `(setq project-switch-commands #'project-dired)` のみ設定する）
- [x] 1.2 `* vertico + orderless + consult` セクションの
      `consult-project-function` 上書き（lambda 版）を削除する
- [x] 1.3 `* marginalia` セクションの projectile 向け
      `marginalia-command-categories` 登録（dolist ブロック）を削除する

## 2. Borg drone の削除

- [x] 2.1 `git submodule deinit -f lib/projectile` を実行する
- [x] 2.2 `git rm lib/projectile` を実行する（`.gitmodules` のエントリも削除される）
- [x] 2.3 `rm -rf .git/modules/projectile` で残骸を削除する

## 3. 検証

- [x] 3.1 `make lisp` で tangle + バイトコンパイルが通ることを確認する
- [x] 3.2 `ERROR_ON_WARN=t make test` が通ることを確認する
- [x] 3.3 `make run-nw` 等で起動し、`C-x p p` の切替 → dired 即時起動、
      切替候補への `embark-act` → `v` で magit-status、
      `consult-ripgrep` のプロジェクトルート検出を手動確認する
