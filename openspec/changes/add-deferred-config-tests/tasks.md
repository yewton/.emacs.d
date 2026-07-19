# add-deferred-config-tests — Tasks

## 1. テスト基盤

- [x] 1.1 `test/test-helper.el` を作成し、`init-test.el` の起動処理（bootstrap → `toncs-init` → `borg-initialize` → `toncs-config-install`）を移す。多重ロード時は no-op になるようガードする
- [x] 1.2 `test/init-test.el` をヘルパー利用に書き換える（既存の `locale-test` は維持）
- [x] 1.3 Makefile の `test` ターゲットを `test/*-test.el` の走査 + 1 プロセス実行に変更する

## 2. 遅延 configure テスト

- [x] 2.1 `test/features-test.el` を作成し、`toncs-config-prepare-functions` から feature 名を導出して feature ごとの ERT テストを動的に定義する（`require` 成功 + `featurep` を検証）

## 3. 検証・ドキュメント

- [x] 3.1 `ERROR_ON_WARN=t make test` で全テストが通ることを確認する（`/verify` スキル相当）
- [x] 3.2 CLAUDE.md の単一テスト実行コマンド例が新構成でも正しいか確認し、必要なら更新する
