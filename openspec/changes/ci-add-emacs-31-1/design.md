## Context

CI (`.github/workflows/ci.yml`) は `purcell/setup-emacs` アクション経由で `purcell/nix-emacs-ci` の Nix flake から Emacs バイナリを取得している。バージョン文字列（例: `31.1`）は `nix-emacs-ci` の flake に定義された固定パッケージ属性（`emacs-31-1`）に対応する必要があり、当該エントリが存在しないバージョンを指定するとビルドが失敗する。

事前調査で、`nix-emacs-ci` の `flake.nix` に `emacs-31-1`（`https://ftp.gnu.org/gnu/emacs/emacs-31.1.tar.xz` を指す固定エントリ）が追加済みであることを確認した。

## Goals / Non-Goals

**Goals:**
- CI マトリクスに正式リリース版 Emacs 31.1 を追加する
- マトリクス内で最古のバージョンである 29.2 を外す

**Non-Goals:**
- `snapshot`（trunk 追跡）の扱いを見直すことは対象外
- コード側の Emacs バージョン分岐（`lisp/toncs-config-magit.org` の `>=29` チェック）の変更は対象外（29.2 を外しても下限チェックとして引き続き有効なため不要）

## Decisions

- `31.1` は `nix-emacs-ci` 側に固定パッケージ（正式リリースタグの tarball）が存在することを確認済みのため採用する。ブランチ tip を追跡する `release-snapshot` は使わない（floating で再現性が落ちるため、固定バージョンが使える以上不要）。
- マトリクスの要素数は現状の4つを維持し、最古のバージョンを1つ外して最新の安定版を1つ追加する運用（`29.2, 30.1, 30.2, snapshot` → `30.1, 30.2, 31.1, snapshot`）とする。

## Risks / Trade-offs

- [Risk] `nix-emacs-ci` の `emacs-31-1` エントリが将来変更・削除された場合、CI が壊れる → 他の固定バージョン（`30.1` 等）と同じ運用であり、既存と同等のリスク。特別な緩和は不要。
