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

### パッケージ管理（Borg）

パッケージは `lib/` 配下の git サブモジュールとして管理されます。Claude Code からの追加は `/add-drone` スキル（`.claude/skills/add-drone`）を使ってください。
