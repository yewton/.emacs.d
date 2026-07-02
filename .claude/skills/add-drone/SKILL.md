---
name: add-drone
description: Borg drone（lib/ 配下の git submodule として管理される Emacs パッケージ）を追加する。「パッケージを導入して」「〜.el を追加して」「drone 追加」等のリクエストで使う。
---

新しい Emacs パッケージを Borg drone として追加する手順。

## 1. drone 名を決める

**GitHub リポジトリ名ではなく、`(require 'X)` する Emacs の feature/package 名を使う。**
この2つはしばしば異なる（例: リポジトリ `akermu/emacs-libvterm` → 名前は `vterm`、
リポジトリ `xenodium/acp.el` → 名前は `acp`）。迷ったらパッケージの主要な `.el`
ファイル末尾の `(provide 'X)` を確認する。

## 2. スクリプトで追加する

```sh
.claude/skills/add-drone/scripts/add-drone.sh <name> <url>
```

内部で以下を行う:

- `git submodule add --name <name> <url> lib/<name>`
- `make -f borg.mk build-fast`（バイトコンパイル + autoloads 生成）し、
  該当 drone のビルドログ部分だけを抜粋表示する

**`--name` を省略してはいけない。** 素の `git submodule add <url> lib/<name>` を実行すると
submodule 名が `lib/<name>` になり、既存 drone 一覧の命名規則（名前のみ、`lib/` プレフィックスなし）
から外れる。このスクリプトは常に `--name` を明示するので、直接 `git submodule add` を
呼ぶのではなく必ずこのスクリプト経由で実行すること。

## 3. 設定ファイルに組み込む

- 他の設定モジュールが依存するライブラリとして使うだけなら、対応する
  `lisp/toncs-config-*.org` の `(require '...)` が並ぶブロックに `(require '<name>)` を追記する。
- 独立した設定モジュールとして追加する場合は `lisp/toncs-config-<name>.org` を新規作成し、
  `lisp/toncs-config.org` に `(toncs-config-prepare <name>)` または
  `(toncs-config-configure <name> ...)` を追加する。

## 4. ビルドして検証する

```sh
make lisp
ERROR_ON_WARN=t make test
```

（`/verify` スキルでも同等の検証ができる）

## 失敗時のやり直し方

`--name` を付け忘れた、あるいは submodule 追加後にビルドや設定で失敗して
最初からやり直したい場合は、コミット前であれば以下でクリーンアップできる:

```sh
git submodule deinit -f lib/<name>
git rm -f lib/<name>
rm -rf .git/modules/lib/<name> .git/modules/<name>
git checkout -- .gitmodules
```

（`.git/modules/` 側のパスは submodule 名によって `lib/<name>` か `<name>` のいずれかになるので
両方 rm しても無害）
