# Design: Migrate projectile to built-in project.el

## Context

現状、プロジェクト操作は Borg drone の projectile（`lib/projectile`）が担っており、
`lisp/toncs-config.org` に以下の依存がある：

- `* projectile` セクション：キャッシュ有効化、ignored-project 除外関数、
  切替後アクション（`projectile-dired`）、`consult-project-function` の上書き（シンボル版）、
  delight、`C-c p` への `projectile-command-map` バインド
- `* vertico + orderless + consult` セクション：`consult-project-function` の上書き（lambda 版）
- `* marginalia` セクション：projectile コマンド向けの `marginalia-command-categories` 登録

事前調査により、これらはすべて project.el（Emacs 29.2+）とエコシステムの
標準機能で代替可能、または不要になることを確認済み（根拠は proposal.md 参照）。

## Goals / Non-Goals

**Goals:**

- projectile への依存を完全に除去し、Borg drone を 1 つ削減する
- プロジェクト操作の設定を最小化する（デフォルトに乗る）
- 「切替候補への embark-act → `v` で magit-status」ワークフローを維持する

**Non-Goals:**

- `C-c p` プレフィクスの互換維持（標準の `C-x p` に移行する）
- `project-vc-merge-submodules` 等のサブモジュール挙動のカスタマイズ
  （デフォルトのまま。不都合が出たら別途調整）
- ignored-project 除外の再実装（project.el は登録がオプトインのため不要）

## Decisions

### D1: projectile セクションを project セクションに置き換える

新しいセクションの設定は次の 1 点のみ：

```emacs-lisp
(toncs-config-configure project
  (setq project-switch-commands #'project-dired))
```

- `project-switch-commands` にコマンドシンボルを直接指定すると、切替時に
  ディスパッチメニューを出さず即実行される（`projectile-switch-project-action` 相当）
- `require` は不要（project.el はビルトインで、コマンドは autoload 済み）。
  ただし `toncs-config-configure` の仕組み上、セクション自体は残して
  `setq` を configure 関数に登録する
- キャッシュ・delight・キーバインドの各設定は代替不要のため設けない

代替案：`toncs-config-prepare project` で遅延設定する案もあるが、
`project-switch-commands` は project.el ロード前に setq しても問題ない
defcustom であり、configure（起動時実行）で十分シンプル。

### D2: consult / marginalia の projectile 連携設定は削除のみ

- `consult-project-function` の上書き 2 箇所を削除。デフォルトの
  `consult--default-project-function` が project.el を使う
- `marginalia-command-categories` への projectile 3 コマンド登録を削除。
  project.el のコマンドは補完テーブル自体が `project-file` カテゴリを持ち、
  marginalia / embark とも標準対応済み

### D3: drone の削除は git submodule の標準手順で行う

```sh
git submodule deinit -f lib/projectile
git rm lib/projectile
rm -rf .git/modules/projectile
```

Borg の drone 削除に特別な手順はなく、submodule の削除がそのまま drone の削除になる。
`.gitmodules` のエントリは `git rm` が自動的に削除する。

### D4: 既知プロジェクトリストは移行しない

projectile の既知プロジェクト（`projectile-known-projects-file`）を project.el の
`project-list-file` に変換するスクリプトは書かない。project.el はプロジェクト内で
コマンドを使えばその場で登録するため、リストは自然に再構築される。

## Risks / Trade-offs

- [`C-c p` の手癖が抜けず一時的に操作ミスが増える] → 意図的な移行。
  `C-x p` は which-key でガイドされるため学習コストは低い
- [projectile 固有コマンド（`projectile-replace` 等）の非互換に後から気づく] →
  設定上バインドしていたのはコマンドマップのみで、個別コマンドへの依存は設定に存在しない。
  必要になれば project.el / rg.el / consult の等価コマンドで代替できる
- [lib/ 配下のサブモジュールが親プロジェクトに統合される挙動の変化] →
  デフォルト（`project-vc-merge-submodules` = t）を受け入れる。
  drone 単体をプロジェクトとして扱いたくなったら nil に変える
- [Emacs 29.2 で project.el の挙動が 30.x と異なる可能性] → 使用機能
  （シンボル値の `project-switch-commands`、`project-dired`、`project-file` カテゴリ）は
  Emacs 28〜29 で導入済み。CI が 29.2 でスモークテストを実行する

## Migration Plan

1. `lisp/toncs-config.org` を編集（projectile → project 置換、consult / marginalia の削除）
2. drone `lib/projectile` を削除
3. `make lisp` で tangle + バイトコンパイル、`ERROR_ON_WARN=t make test` で検証
4. ロールバックは git revert + `git submodule update --init` で可能

## Open Questions

（なし）
