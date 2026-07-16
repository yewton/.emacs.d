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

## 2. Epkg データベースで正規リポジトリを確認する

```sh
.claude/skills/add-drone/scripts/lookup.sh <name>
```

`borg-assimilate` が参照するのと同じ Epkg データベース（emacsmirror の
パッケージ定義、`var/epkgs`）から、正規の upstream URL・summary・必須依存を
引く。**リポジトリ URL を Web 検索で探してはいけない。** 同名の別パッケージや
fork を正規リポジトリと取り違える事故の元になる。

- 出力の `url:` をそのまま手順 3 に渡す
- class が wiki / orphaned の場合は供給元の信頼性に注意が必要なので、
  進める前にユーザーに確認する
- 必須依存に「未登録」があれば、その依存パッケージも本手順（1〜3）で追加する
- 見つからない場合は、まずデータベース自体を更新して再確認する:

  ```sh
  .claude/skills/add-drone/scripts/update-epkgs.sh
  ```

  （emacs --batch から `epkg-update` を実行し、パッケージリストと定義を
  最新化する。）それでも見つからない新しすぎる・マイナーなパッケージに
  限り、upstream リポジトリを自力で特定し、URL をユーザーに確認してから
  手順 3 に進む。

## 3. スクリプトで追加する

```sh
.claude/skills/add-drone/scripts/add-drone.sh <name> <url>
```

内部で emacs --batch から **`borg-assimilate`** を呼ぶ。submodule の登録・
`.gitmodules` の整列・git dir の absorb・drone のビルドまで borg が一貫して
行うので、**`git submodule add` を直接叩いてはいけない**（`--name` の
付け忘れなど手作業の事故を防ぎ、borg の管理下で完結させるため）。
変更はステージされた状態で残る（コミットはしない）。

## 4. 設定ファイルに組み込む

- 他の設定モジュールが依存するライブラリとして使うだけなら、対応する
  `lisp/toncs-config-*.org` の `(require '...)` が並ぶブロックに `(require '<name>)` を追記する。
- 独立した設定モジュールとして追加する場合は `lisp/toncs-config-<name>.org` を新規作成し、
  `lisp/toncs-config.org` に `(toncs-config-prepare <name>)` または
  `(toncs-config-configure <name> ...)` を追加する。

## 5. ビルドして検証する

```sh
make lisp
ERROR_ON_WARN=t make test
```

（`/verify` スキルでも同等の検証ができる）

## 失敗時のやり直し方

assimilate 後にビルドや設定で失敗して最初からやり直したい場合は、
コミット前であれば以下でクリーンアップできる:

```sh
git submodule deinit -f lib/<name>
git rm -f lib/<name>
rm -rf .git/modules/lib/<name> .git/modules/<name>
git checkout -- .gitmodules
```

（`.git/modules/` 側のパスは submodule 名によって `lib/<name>` か `<name>` のいずれかになるので
両方 rm しても無害）
