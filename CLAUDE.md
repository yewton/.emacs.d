# CLAUDE.md

このファイルは、リポジトリ内のコードを扱う Claude Code (claude.ai/code) へのガイダンスを提供します。

## 概要

**㌧ｸｽ** — yewton の個人 Emacs 設定 (`~/.emacs.d`) です。Org mode を使った Literate Emacs Config であり、設定は `.org` ファイルに記述され、`org-babel-tangle` によって `.el` ファイルに変換されます。パッケージは [Borg](https://emacsmirror.net/manual/borg)（`lib/` 配下の git サブモジュール）で管理されます。

対応 Emacs バージョンは @.github/workflows/ci.yml の `emacs_version` マトリクスを参照してください。

## 作業スタイル

- 実装前に計画を提案し、承認を得てから着手すること
- 回答は簡潔に

## よく使うコマンド

```sh
# .org ファイルを tangle して .el を生成し、lisp/ をバイトコンパイル
make

# テスト実行（バッチ Emacs で ERT）
make test

# tangle のみ（バイトコンパイルをスキップ）
make lisp

# 生成ファイルの削除
make clean

# 既存の ~/.emacs.d を汚さずにこの設定で Emacs を起動
make run

# Borg サブモジュールのブートストラップ（クローン直後に必要）
make -f borg.mk bootstrap-fast
```

`ERROR_ON_WARN=t make test` はバイトコンパイル警告をエラーとして扱います（CI と同様）。

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

### ディレクトリ構成

- `lib/` — Borg ドローンパッケージ（git サブモジュール）
- `lisp/` — ユーザ設定の `.org` ファイルと生成された `.el` ファイル
- `var/` — ランタイムデータ（SKK 辞書、el-get、ellama セッション等）
- `test/` — ERT テスト（`init-test.el`）
- `res/` — 静的アセット（Org agenda 用 SVG アイコン）
- `etc/` — その他の設定データ
- `tree-sitter/` — tree-sitter 文法ファイル
