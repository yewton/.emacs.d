## Why

LSP / eglot / ggtags のいずれも導入していないため、非 elisp のプログラミング言語では
`M-.`（定義ジャンプ）が実質的に機能しない（TAGS がなければ etags バックエンドが空振りする）。
dumb-jump は ripgrep で定義候補をヒューリスティックに探す軽量な補完手段で、
言語ごとのサーバやタグ生成なしに「とりあえず定義へ飛べる」状態を作れる。

既存の `M-.` = `embark-dwim` は、非 elisp のコードバッファ上では
`embark-identifier-map` の既定アクション `xref-find-definitions` に委譲される。
そのため dumb-jump を **xref のフォールバックバックエンド**として登録するだけで、
キーバインドを一切変えずに既存導線へ載せられる（検証済み・後述）。

## What Changes

- dumb-jump の Borg drone（`lib/dumb-jump`）を追加する。
  実ファイル確認の結果、現行バージョン（0.5.5）は `dash` / `s` / `popup` いずれにも
  依存しておらず（Emacs 組み込みの `xref` / `cl-generic` / `cl-lib` / `seq` / `subr-x` / `json` のみ）、
  追加の依存 drone は不要だった
- `dumb-jump-xref-activate` を `xref-backend-functions` に登録し、
  xref のフォールバックバックエンドとして dumb-jump を有効化する
- searcher を ripgrep に固定する（`dumb-jump-force-searcher` を `'rg`）。
  設定全体が rg 中心（consult-ripgrep / rg.el）であることと揃える
- 遅延ロード方針を守る：`dumb-jump-xref-activate` は autoload 済みのため、
  起動時に dumb-jump 本体はロードせず、実際に xref を叩いたとき初めてロードする

## Capabilities

### New Capabilities

- `code-navigation`: xref を用いた定義ジャンプにおいて、専用バックエンド
  （LSP 等）を持たない言語で dumb-jump をフォールバックとして使えることの要件。
  既存の `M-.`（embark-dwim → xref）導線・`consult-xref` 表示との一貫性、
  および elisp バッファでの非介入を含む

### Modified Capabilities

（なし）

## Impact

- `lisp/toncs-config.org`（dumb-jump 登録セクションの追加）と tangle 生成物
- `lisp/toncs-config-dumb-jump.org` を新規作成（configure 側で `dumb-jump-force-searcher` を設定）
- `.gitmodules` と `lib/dumb-jump`（サブモジュール追加。`popup` 等の追加依存 drone は不要と確認済み）
- 遅延ロードされる全設定モジュールの configure を検証する既存テスト
  （`test/features-test.el`）に dumb-jump モジュールが追加される
- Emacs 最小サポートバージョン（29.2）は変更不要：xref のフォールバックバックエンド
  登録・`dumb-jump-xref-activate` は対象バージョンすべてで利用可能。README / CI も変更不要
- ユーザー影響：キーバインド変更なし。非 elisp 言語で `M-.` が
  dumb-jump 経由で定義候補へ飛べるようになる（elisp は従来どおり `embark-find-definition`）
