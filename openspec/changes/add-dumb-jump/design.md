## Context

この Emacs 設定は LSP / eglot / ggtags / etags いずれの「賢い」xref バックエンドも
導入していない。elisp は `elisp--xref-backend` で定義ジャンプできるが、
それ以外の言語では `M-.` が空振りする。

`M-.` はグローバルで `embark-dwim` に再割り当てされている（`toncs-config.org`）。
embark のターゲット判定を追うと、**非 elisp のコードバッファ**では
`embark-target-identifier-at-point` が `identifier` 型を返し、`embark-identifier-map` の
既定アクション（`RET`）である `xref-find-definitions` に委譲される。
**elisp バッファ**では `symbol` 型となり `embark-symbol-map` の
`embark-find-definition`（`find-function` 系）が使われる。

したがって dumb-jump を xref のフォールバックバックエンドとして登録すれば、
既存の `M-.` 導線にキーバインド変更なしで載る。候補表示も
`xref-show-definitions-function` = `consult-xref` に設定済みのため一貫する。

パッケージは Borg drone（`lib/` 配下のサブモジュール）で管理する。
設定は Literate（`.org` を tangle）。遅延ロードは `toncs-config-configure` /
`toncs-config-prepare` マクロで制御する。CI は `ERROR_ON_WARN=t` でバイトコンパイル
警告をエラー扱いにする。

## Goals / Non-Goals

**Goals:**
- 専用バックエンドを持たない言語で `M-.` から定義候補へ飛べるようにする
- 既存のキーバインド・候補表示 UI を変更しない
- elisp の既存ジャンプ挙動に干渉しない
- 起動時に dumb-jump 本体をロードしない（遅延ロード）
- searcher を ripgrep に固定する

**Non-Goals:**
- LSP / eglot の導入（本変更のスコープ外）
- dumb-jump 独自コマンド（`dumb-jump-go` 系）へのキー割り当て
- タグ生成・言語ごとのプロジェクト設定（`.dumbjump`）の整備
- xref 以外の入口（例：補完 capf）への dumb-jump 連携

## Decisions

### 決定 1: xref フォールバックバックエンドとして登録する
`(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)` を採用する。

- `xref-backend-functions` は `run-hook-with-args-until-success` で評価される。
  emacs-lisp-mode 等はバッファローカルに専用バックエンドを追加するため、
  グローバルに追加した dumb-jump より**バッファローカルが先に評価される**。
  よって elisp では `elisp--xref-backend` が勝ち、dumb-jump は介入しない。
- `add-hook` の既定（prepend）でグローバルの `etags--xref-backend` より前に置く。
  `etags--xref-backend` は常に非 nil を返すため、append すると dumb-jump が
  永久に shadow される。prepend なら dumb-jump が「対応言語かつプロジェクトあり」の
  ときだけ採用し、それ以外は nil を返して etags へ素通りする。
- **代替案（不採用）**: `dumb-jump-go` 等を直接キーに割り当てる方式。
  `M-.` = embark-dwim を潰すか別キーを新設することになり、
  「既存と競合しない」要件と候補表示の一貫性（consult-xref）を損なう。

### 決定 2: searcher は ripgrep に固定
`dumb-jump-force-searcher` を `'rg` に設定する。

- dumb-jump は git リポジトリ内では既定で git-grep を選ぶ。この設定の対象リポジトリは
  ほぼ git 管理下のため、rg を使わせるには `prefer-searcher` では不十分で
  `force-searcher` が要る。
- rg は導入済みで、設定全体（consult-ripgrep / rg.el）と揃う。
- **代替案（不採用）**: 既定（git-grep）のまま。動作はするが rg 中心の設定と不揃いで、
  submodule 配下（`lib/`）など tracked 範囲外の探索で挙動が変わりうる。

### 決定 3: モジュール構成と遅延ロード（`toncs-config-prepare`）
`toncs-config.org` に登録セクションを追加し、`lisp/toncs-config-dumb-jump.org` を新設する。

- 登録側（即時評価される BODY）でフックを張る:
  ```elisp
  (toncs-config-prepare dumb-jump
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))
  ```
  `dumb-jump-xref-activate` は autoload 済みなので、フック登録だけでは本体をロードしない。
- configure 側（`with-eval-after-load 'dumb-jump` で実行）で defcustom を設定:
  ```elisp
  ;; toncs-config-dumb-jump.org
  (require 'dumb-jump)
  (defun toncs-config-dumb-jump-configure ()
    (setq dumb-jump-force-searcher 'rg))
  ```
  モジュール冒頭で `(require 'dumb-jump)` するため、`dumb-jump-force-searcher` を
  setq してもバイトコンパイルの free-variable 警告が出ない（`ERROR_ON_WARN` 対策）。
- **代替案（不採用）**: `toncs-config-configure` 内で `(require 'dumb-jump)` して即時設定。
  起動時に本体をロードしてしまい遅延ロード方針に反する。

### 決定 4: 依存 drone
`dumb-jump` drone のみを追加する。実ファイル（`lib/dumb-jump/dumb-jump.el`、バージョン 0.5.5）で
確認した結果、`(require 'popup)` は存在せず、依存は Emacs 組み込みの
`xref` / `cl-generic` / `cl-lib` / `seq` / `subr-x` / `json` のみだった。
`dash` / `s` / `popup` いずれも drone として追加する必要はない。

## Risks / Trade-offs

- **[dumb-jump はヒューリスティック探索で誤爆・取りこぼしがある]** → LSP の代替ではなく
  「専用バックエンドがない言語向けのフォールバック」と位置づける。elisp や将来 LSP を
  入れた言語ではバッファローカルバックエンドが優先されるため影響しない。
- **[`etags--xref-backend` によるフォールバック順の shadow]** → prepend で登録し、
  テストで `xref-backend-functions` への登録を検証する。
- **[popup を hard require する dumb-jump のバージョン依存]** → drone 追加時に
  実ファイルで `(require 'popup)` の有無を確認した。バージョン 0.5.5 では該当なしと判明し、
  popup drone の追加は不要だった。
- **[configure が `with-eval-after-load` 依存で、初回 activate 時の設定タイミング]** →
  実際の探索は activate 後の `xref-backend-definitions` で走り、その前にロード＝configure が
  完了するため、初回から force-searcher が効く。

## Migration Plan

1. `/add-drone` で `dumb-jump` を追加し（追加依存 drone は不要と確認済み）`make -f borg.mk build-fast`
2. `toncs-config.org` に `toncs-config-prepare dumb-jump` セクションを追加
3. `lisp/toncs-config-dumb-jump.org` を新設し configure を実装
4. `test/features-test.el` に dumb-jump モジュールを追加
5. `ERROR_ON_WARN=t make test`（`/verify`）で検証
6. ロールバック：drone 削除と両セクションの除去で元へ戻せる（他機能へ影響なし）

## Open Questions

- なし（searcher = rg、xref フォールバック方式で確定）
