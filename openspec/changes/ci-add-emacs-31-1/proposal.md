## Why

Emacs 31.1 が正式リリースされた。CI の `emacs_version` マトリクスは `[29.2, 30.1, 30.2, snapshot]` のままで、最新の安定版である 31.1 をテストしていない。また、マトリクス内の最古のバージョンである 29.2 を外すことも合わせて行いたい。

## What Changes

- `.github/workflows/ci.yml` の `emacs_version` マトリクスを `[29.2, 30.1, 30.2, snapshot]` から `[30.1, 30.2, 31.1, snapshot]` に変更する
  - `29.2` を削除する（マトリクス内の最古のバージョンを外す）
  - `31.1` を追加する（`purcell/nix-emacs-ci` に `emacs-31-1`（`emacs-31.1.tar.xz` を指す固定エントリ）が追加済みであることを確認済み）
- `README.org` の `Supports-Emacs_29.x` バッジを `Supports-Emacs_30.x` に更新する（CI がテストする最小バージョンが 29.2 → 30.1 になるため）

## Capabilities

### New Capabilities

- `ci-emacs-version-matrix`: CI がテスト対象とする Emacs バージョンの組み合わせに関する要件

### Modified Capabilities

（なし）

## Impact

- 影響ファイル: `.github/workflows/ci.yml`, `README.org`
- コード側（`lisp/*.org`）に 29 系限定の分岐は `lisp/toncs-config-magit.org` の `(version< emacs-version "29")` のみで、これは下限チェックのため 29.2 を外しても変更不要と確認済み
