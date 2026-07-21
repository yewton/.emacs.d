## 1. drone 追加

- [x] 1.1 `change/add-dumb-jump` ブランチを作成する
- [x] 1.2 `/add-drone` で `dumb-jump` を追加する
- [x] 1.3 追加された `lib/dumb-jump/dumb-jump.el` を確認し、`(require 'popup)` があれば `/add-drone` で `popup` も追加する（`dash` / `s` は既存）
- [x] 1.4 `make -f borg.mk build-fast` で drone をビルドする

## 2. 設定モジュールの実装

- [x] 2.1 `lisp/toncs-config-dumb-jump.org` を新設し、冒頭で `(require 'dumb-jump)`、`toncs-config-dumb-jump-configure` で `dumb-jump-force-searcher` を `'rg` に設定する
- [x] 2.2 `toncs-config.org` に `(toncs-config-prepare dumb-jump ...)` セクションを追加し、BODY で `(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)` を登録する
- [x] 2.3 `make lisp` で tangle + バイトコンパイルが通ることを確認する

## 3. テスト

- [x] 3.1 `test/features-test.el` に dumb-jump モジュールの configure 検証を追加する
- [x] 3.2 起動直後に feature `dumb-jump` が未ロード（`featurep` が nil）であること、および `dumb-jump-xref-activate` が `xref-backend-functions` に登録されることを検証する
- [x] 3.3 `ERROR_ON_WARN=t make test`（`/verify`）で CI 相当のテストを通す

## 4. 仕上げ

- [ ] 4.1 変更をコミットし Pull Request を作成する（マージはユーザが行う）
