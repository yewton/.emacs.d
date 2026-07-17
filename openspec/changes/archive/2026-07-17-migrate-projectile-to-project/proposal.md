# Migrate projectile to built-in project.el

## Why

プロジェクト操作（切替・ファイル検索・grep 等）に外部パッケージ projectile を使っているが、
設定が projectile に依存している機能は薄く、すべてビルトインの project.el で代替できる。
外部依存（Borg drone）を 1 つ減らし、設定も簡素化できる。

検証済みの根拠：

- consult 連携：`consult-project-function` のデフォルト
  （`consult--default-project-function`）が project.el を使うため、上書きは削除するだけでよい
- marginalia / embark 連携：project.el の切替補完（`project-prompt-project-dir`）は
  ネイティブに `project-file` カテゴリを持ち、marginalia は標準アノテータで対応済み、
  embark は標準変換（`embark--project-file-full-path`）で候補（絶対パス）を
  `file` ターゲットとして扱う。「プロジェクト切替候補への embark-act → `v`
  （embark-magit-status）」ワークフローは手動カテゴリ登録なしで動作する
- 切替後の dired 起動：`project-switch-commands` にコマンドシンボルを直接指定すれば
  ディスパッチメニューなしで `project-dired` が即実行される（docstring に明記）
- ファイル一覧キャッシュ：project.el は `git ls-files` ベースで十分速く、キャッシュ設定は不要
- ignored-project 除外（no-littering の var/ と /usr/local）：projectile は訪問した
  全プロジェクトを自動登録するため除外が必要だったが、project.el はプロジェクトコマンドを
  実行したときだけ登録する（オプトイン）ため、除外設定自体が不要

## What Changes

- toncs-config.org の projectile セクションを project.el ベースの設定に置き換える
  - `project-switch-commands` に `#'project-dired` を直接指定
  - キーバインドは標準の `C-x p`（`project-prefix-map`）に合わせ、`C-c p` は廃止
  - `project-vc-merge-submodules` 等はデフォルトのまま（不都合があれば後から調整）
  - ignored-project 相当の設定（`toncs-projectile-ignored-project-function`）は移行しない
- consult セクションの `consult-project-function` 上書き（lambda 版）を削除する
  （projectile セクションにあるシンボル版の上書きはセクションごと削除される）
- marginalia セクションの projectile 向け `marginalia-command-categories` 登録を削除する
- projectile の Borg drone（lib/projectile）を削除する

## Capabilities

### New Capabilities

- `project-workflow`: プロジェクトの切替・ファイル検索・grep と、
  切替候補に対する embark 連携（magit-status 起動）の要件

### Modified Capabilities

（なし — 既存 spec はまだ存在しない）

## Impact

- `lisp/toncs-config.org`（projectile / consult / marginalia セクション）と
  tangle 生成物 `lisp/toncs-config.el`
- `.gitmodules` と `lib/projectile`（サブモジュール削除）
- Emacs 最小サポートバージョン（29.2）は変更不要：使用する project.el の機能
  （`project-switch-commands` のシンボル値、`project-dired`、切替補完の
  `project-file` カテゴリ）はすべて Emacs 29 以前から存在する。README / CI も変更不要
- ユーザー影響：**プロジェクト操作のプレフィクスキーが `C-c p` から `C-x p` に変わる**。
  また lib/ 配下のサブモジュールは（デフォルトの `project-vc-merge-submodules` により）
  親リポジトリと同一プロジェクト扱いになる
