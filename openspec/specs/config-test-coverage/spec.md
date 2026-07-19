# config-test-coverage

## Purpose

設定モジュールのテストカバレッジに関する要件。起動経路のスモークテストに加え、
遅延ロードされる全設定モジュールの configure 実行を CI で検証する。

## Requirements

### Requirement: 遅延 configure は全 feature 分がテストで実行される
`toncs-config-prepare` で登録されたすべての feature について、その feature をロードし
遅延 configure（`toncs-config-FEATURE-configure`）がエラーなく実行完了することを
ERT テストで検証しなければならない（MUST）。テスト対象の feature 一覧は
`toncs-config-prepare-functions` の登録内容から動的に導出され、
ハードコードされたリストを持ってはならない（MUST NOT）。

#### Scenario: 全 feature のロード検証
- **WHEN** `make test` を実行する
- **THEN** 登録済みの各 feature に対応する ERT テストが実行され、それぞれの feature が `require` によりロードされ configure が完了する

#### Scenario: モジュール追加への自動追従
- **WHEN** 新しい設定モジュールを `toncs-config-prepare` で登録して `make test` を実行する
- **THEN** テストコードを変更することなく、その feature のロード検証テストが実行される

#### Scenario: configure 本体の実行時エラーの検出
- **WHEN** いずれかの feature の configure 本体が実行時エラーを起こす状態で `make test` を実行する
- **THEN** 該当 feature のテストが失敗し、テスト名から失敗した feature が特定できる

### Requirement: テスト共通の起動処理はヘルパーに集約される
bootstrap から `toncs-config-install` までの起動処理は `test/test-helper.el` に
集約されなければならない（MUST）。ヘルパーは複数回ロードされても起動処理を
再実行しない（MUST）。各テストファイルはこのヘルパーを利用する。

#### Scenario: 複数テストファイルの共存
- **WHEN** 複数のテストファイルが同一 Emacs プロセスにロードされる
- **THEN** 起動処理は 1 回だけ実行され、すべてのテストが実行される

### Requirement: make test は test/ 配下の全テストファイルを実行する
`make test` は `test/` 配下のすべてのテストファイル（`*-test.el`）をロードして
ERT テストを実行しなければならない（MUST）。テストファイルの追加時に
Makefile の変更を必要としてはならない（MUST NOT）。

#### Scenario: テストファイルの追加
- **WHEN** `test/` に新しい `*-test.el` を追加して `make test` を実行する
- **THEN** Makefile を変更することなく、新しいファイル内のテストが実行される
