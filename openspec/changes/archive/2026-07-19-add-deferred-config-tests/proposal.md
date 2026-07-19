# add-deferred-config-tests

## Why

現在のテストは `test/init-test.el` によるスモークテスト 1 本のみで、起動経路（bootstrap → `borg-initialize` → `toncs-config-install`）しか検証していない。`toncs-config-prepare` で登録された 21 モジュールの configure 本体は `with-eval-after-load` で遅延されるため、バッチテストでは一度も実行されず、CI ではバイトコンパイル警告しか検出できない。この設定の変更の主因は週次の drone 自動更新と Emacs バージョン更新であり、「パッケージ更新で configure 本体が実行時エラーになる」というリスクが無検証のまま残っている。

事前スパイクにより、全 21 feature（vterm・skk・ef-themes 含む）がバッチ Emacs で `require` 可能で、追加コストは合計約 2.4 秒であることを確認済み。

## What Changes

- `test/` を複数テストファイル構成にし、bootstrap から `toncs-config-install` までの共通起動処理をテストヘルパーに切り出す
- `toncs-config-prepare-functions` に登録された各 feature を `require` し、遅延 configure がエラーなく実行されることを検証するテストを追加する（登録リストから動的にテストを生成し、モジュール追加時の追従を不要にする）
- Makefile の `test` ターゲットを複数テストファイルの実行に対応させる

## Capabilities

### New Capabilities

- `config-test-coverage`: 設定モジュールのテストカバレッジに関する要件。起動経路のスモークテストに加え、遅延ロードされる全設定モジュールの configure 実行を CI で検証する

### Modified Capabilities

（なし — 既存 spec `project-workflow` の要件に変更はない）

## Impact

- `test/` — 新規ヘルパー・テストファイルの追加、既存 `init-test.el` のヘルパー利用への書き換え
- `Makefile` — `test` ターゲットの変更
- `CLAUDE.md` — 単一テスト実行コマンド例の更新（ヘルパー構成に伴う場合のみ）
- CI（`.github/workflows/ci.yml`）は `make test` を呼ぶだけなので変更不要
