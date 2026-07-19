# CLAUDE.md

このファイルは、リポジトリ内のコードを扱う Claude Code (claude.ai/code) へのガイダンスを提供します。

## 概要

**㌧ｸｽ** — yewton の個人 Emacs 設定 (`~/.emacs.d`) です。Org mode を使った Literate Emacs Config であり、設定は `.org` ファイルに記述され、`org-babel-tangle` によって `.el` ファイルに変換されます。パッケージは [Borg](https://emacsmirror.net/manual/borg)（`lib/` 配下の git サブモジュール）で管理されます。

対応 Emacs バージョンは @.github/workflows/ci.yml の `emacs_version` マトリクスを参照してください。

## 作業スタイル

- 実装前に計画を提案し、承認を得てから着手すること
- 回答は簡潔に
- OpenSpec change（`/opsx:*`）の実装は master へ直接コミットせず、change 名を含むブランチ（例: `change/<change-name>`）で作業し、Pull Request として提出すること。マージはユーザが行う

## よく使うコマンド

```sh
# .org ファイルを tangle して .el を生成し、lisp/ をバイトコンパイル（+ 辞書ファイル生成）
# drone が index の記録とずれていれば git submodule update --init と drone のビルドも行う
# （意図的に進めた drone は git add lib/<drone> でステージしておけば同期対象にならない）
make

# tangle + バイトコンパイル（辞書ファイル生成を除く）
make lisp

# テスト実行（バッチ Emacs で ERT）
make test

# 単一テストの実行（SELECTOR は ERT セレクタ。テスト名の正規表現など。事前に make でビルドしておくこと）
emacs --batch --load ert --load test/init-test.el --load test/features-test.el --eval '(ert-run-tests-batch-and-exit "SELECTOR")'

# ターミナル内でこの設定の Emacs を起動（make run の -nw 版）
make run-nw

# 生成ファイルの削除
make clean

# 既存の ~/.emacs.d を汚さずにこの設定で Emacs を起動
make run

# Borg サブモジュールのブートストラップ（クローン直後に必要）
make -f borg.mk bootstrap-fast
```

`ERROR_ON_WARN=t make test` はバイトコンパイル警告をエラーとして扱います（CI と同様）。`/verify` スキルはこのコマンドを実行します。

### .org ファイルでの特殊文字

PUA 領域（NerdFont 等、U+E000〜U+F8FF や U+F0000〜）の文字を `.org` ファイルに書く場合、文字リテラル（`?文字`）は書き込みツールによって欠落する場合があります。数値コードポイント（例: `#xf444`）を使ってください。

## アーキテクチャ

### Literate Config の流れ

1. `README.org` — `toncs-bootstrap.el`、`early-init.el`、`init.el` に tangle される
2. `lisp/*.org` — それぞれ `lisp/*.el` に tangle される
3. `setup.org` — 全 `.org` ファイル共通の `#+property` ヘッダ（tangle デフォルト設定）

**`lisp/` 内の `.el` ファイルは直接編集しない** — 自動生成ファイルです。対応する `.org` ファイルを編集してください。

### 主要ソースファイル

| ファイル | 用途 |
|---|---|
| `lisp/toncs-stdlib.org` | 外部依存なしのコアユーティリティ。`toncs-init`、ロードパスヘルパーを定義 |
| `lisp/toncs-config.org` | メイン設定。`toncs-config-configure` / `toncs-config-prepare` マクロを定義し `toncs-config-install` を呼び出す |
| `lisp/toncs-config-*.org` | 機能別設定モジュール。`with-eval-after-load` で遅延ロード |
| `toncs-bootstrap.el` | `user-emacs-directory`、ロードパスを設定し `package.el` を無効化 |
| `early-init.el` | フレームデフォルト、GC チューニング |
| `init.el` | Bootstrap → Borg 初期化 → `toncs-config-install` |
| `custom.el` | Emacs の `customize` 出力（`.org` には含まれない） |

### 設定マクロ

`toncs-config.org` の2つのマクロが遅延ロードを制御します：

- **`toncs-config-configure THEME &rest BODY`** — `toncs-config-THEME-configure` を定義し `toncs-config-configure-functions` に登録します。`toncs-config-install` 実行時に呼ばれます。
- **`toncs-config-prepare FEATURE &rest BODY`** — `toncs-config-FEATURE` から `toncs-config-FEATURE-configure` を autoload し、`FEATURE` のロード後に実行されるよう設定します。BODY は即時評価されます（キーバインド設定など）。

`toncs-config-install` は登録された configure / prepare 関数をすべて実行します。

### コーディング規則

- 独自シンボルには `toncs-` プレフィクスを付ける（例: `toncs-config-configure`）

### パッケージ管理（Borg）

パッケージは `lib/` 配下の git サブモジュールとして管理されます。追加方法：

```sh
# Emacs 内から:
M-x borg-assimilate
# または手動で:
git submodule add <url> lib/<pkg>
make -f borg.mk build-fast
```

Claude Code からの追加は `/add-drone` スキル（`.claude/skills/add-drone`）を使ってください。

### ディレクトリ構成

- `lib/` — Borg ドローンパッケージ（git サブモジュール）
- `lisp/` — ユーザ設定の `.org` ファイルと生成された `.el` ファイル
- `var/` — ランタイムデータ（SKK 辞書、el-get、ellama セッション等）
- `test/` — ERT テスト。`test-helper.el` が bootstrap から `toncs-config-install` までの起動処理を共通ヘルパーとして提供し、`init-test.el`（起動スモークテスト）と `features-test.el`（遅延ロードされる全設定モジュールの configure 実行を検証）がこれを利用する
- `openspec/` — OpenSpec による変更提案・スペック管理（`/opsx:*` スキルが使用）
- `res/` — 静的アセット（Org agenda 用 SVG アイコン）
- `etc/` — その他の設定データ
- `tree-sitter/` — tree-sitter 文法ファイル
