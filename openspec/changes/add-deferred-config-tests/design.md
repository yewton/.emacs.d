# add-deferred-config-tests — Design

## Context

テストは `test/init-test.el` 1 ファイルのみで、ファイル冒頭のトップレベルフォームで bootstrap → `borg-initialize` → `toncs-config-install` を実行し、ERT テストは locale 確認 1 件だけを定義している。Makefile の `test` ターゲットはこのファイルを直接 `--load` している。

`toncs-config-prepare` は feature ごとに `toncs-config-FEATURE-prepare` 関数を定義して `toncs-config-prepare-functions` に登録し、その中で `with-eval-after-load` により `toncs-config-FEATURE-configure` の遅延実行を仕込む。バッチテストでは対象 feature がロードされないため、configure 本体は実行されない。

事前スパイク（バッチ Emacs で全 21 feature を `require`）で以下を確認済み：

- 全 feature がエラーなくロード・configure 完了する（skip リスト不要）
- 追加時間は合計約 2.4 秒（最重: magit 0.68s、org 0.49s）
- CI 環境でも vterm モジュールは cmake によりビルドされ、SKK 辞書は `make test` が依存する `all` ターゲットで生成されるため、成立する見込み

## Goals / Non-Goals

**Goals:**

- 遅延ロードされる全設定モジュールの configure 本体を CI で実行・検証する
- モジュール追加・削除時にテストコードの追従を不要にする（登録リスト駆動）
- テストファイルを追加しやすい基盤（共通ヘルパー + 複数ファイル走査）を整える

**Non-Goals:**

- 個別関数のユニットテスト（`toncs-find-font` 等）— 将来の変更で必要になったら追加する
- キーバインド等の起動後インバリアントの網羅検証
- テスト実行時の `var/` 実データからの隔離（現行スモークテストと同等の挙動を維持。CI ではクリーンな `var/` のため実害なし）

## Decisions

### D1: feature 一覧は `toncs-config-prepare-functions` から動的に導出する

登録済み関数名 `toncs-config-FEATURE-prepare` から FEATURE 名を抽出し、feature ごとに ERT テストを動的に定義する（マクロまたは `dolist` + `ert-set-test` 相当）。ハードコードされた feature リストは持たない。

- 理由: モジュール追加時にテストの更新忘れが起きない。「登録されているのに検証されない」状態を構造的に排除する
- 代替案: feature 名を列挙した静的リスト → ドリフトするため不採用

### D2: 検証は `require` の成功をもって行う

`with-eval-after-load` のフックは `require` 完了時に同期的に実行され、configure 内のエラーはそのまま伝播する。したがって `(require FEATURE)` がエラーなく完了すれば configure 本体の実行成功を意味する。テストは feature ごとに 1 件とし、`should` で `(featurep FEATURE)` を確認する。

- 理由: 仕組みが単純で、失敗時にどの feature かがテスト名から即座に分かる
- 注意: 起動時に既にロード済みの feature（例: `paren`）は configure が `toncs-config-install` 時点で実行済みだが、その場合もエラーは起動段階で顕在化するため検証意図は満たされる

### D3: skip リストは設けない（必要になるまで）

スパイクで全 feature がバッチロード可能と確認済みのため、除外機構は実装しない。将来 CI 環境差などで除外が必要になった場合に、明示的な skip リスト（デフォルトは全件対象）を導入する。

- 理由: YAGNI。使われない機構は誤った安心感を生む

### D4: 共通起動処理は `test/test-helper.el` に切り出す

bootstrap → `toncs-init` → `borg-initialize` → `toncs-config-install` のトップレベル処理を `test/test-helper.el` に移し、各テストファイルは冒頭でこれをロードする。ヘルパーは多重ロードしても安全にする（2 回目以降は no-op）。

- 理由: テストファイルを増やしても起動処理が重複実行されない。既存 `init-test.el` との共通化
- パス解決: 既存実装と同じく `locate-dominating-file` で `init.el` を探す方式を踏襲する

### D5: Makefile は `test/*-test.el` を走査して 1 プロセスで実行する

`test` ターゲットは `test/` 配下の `*-test.el` をすべて `--load` した上で `ert-run-tests-batch-and-exit` を 1 回実行する。

- 理由: 起動処理（borg 初期化含む）が重いため、ファイルごとのプロセス分離はコストに見合わない。ヘルパーの多重ロード防止（D4）により 1 プロセスで安全に共存できる
- 代替案: ファイルごとに Emacs プロセスを起動 → 起動コスト × ファイル数となり不採用

## Risks / Trade-offs

- [CI 環境で特定 feature のロードが失敗する（ローカルとの環境差）] → CI マトリクス（ubuntu/macos × Emacs 4 バージョン）での初回実行で洗い出す。恒常的に失敗する環境依存 feature が見つかった場合のみ D3 の skip リストを導入する
- [feature ロードが `var/` 等に副作用を残す（org-roam DB 生成など）] → 現行スモークテストと同等のリスク。CI は使い捨て環境のため実害なし。ローカルでは `make test` 実行が既に同種の副作用を持つ
- [1 プロセス実行のため、先行テストのロードが後続テストの前提を汚染しうる] → 現状のテストは「ロードが成功すること」の検証が主目的のため許容。将来、隔離が必要なテストが出た場合は別ターゲットに分離する

## Open Questions

（なし）
