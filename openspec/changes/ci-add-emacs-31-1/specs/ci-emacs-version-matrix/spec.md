## ADDED Requirements

### Requirement: CI は現行の Emacs 安定版マトリクスをテストする
CI (`.github/workflows/ci.yml`) は `emacs_version` マトリクスとして、直近2つのマイナーバージョン系列の最新パッチリリースと、その次の未リリースバージョン系列の最新パッチリリース、および `snapshot`（trunk 追跡）を対象としなければならない（MUST）。マトリクスの要素数は4を維持する。

#### Scenario: 新しい Emacs マイナーバージョンがリリースされたとき
- **WHEN** 新しい Emacs マイナーバージョン系列の最新パッチリリースが `purcell/nix-emacs-ci` の固定パッケージとして利用可能になる
- **THEN** マトリクスに当該バージョンを追加し、マトリクス内で最古のバージョンを外す

#### Scenario: 固定バージョンが nix-emacs-ci に未追加のとき
- **WHEN** 対象の Emacs バージョンに対応する固定パッケージ（例: `emacs-31-1`）が `purcell/nix-emacs-ci` の flake にまだ定義されていない
- **THEN** そのバージョンをマトリクスに追加しない（ビルド失敗を避けるため、`release-snapshot` 等の floating なエントリで代替しない）

### Requirement: README のバッジは CI がテストする最小バージョンと一致する
`README.org` の `Supports-Emacs_*` バッジは、CI の `emacs_version` マトリクス内で最小の安定版バージョンのマイナー系列を表示しなければならない（MUST）。

#### Scenario: マトリクスの最古バージョンが入れ替わったとき
- **WHEN** `emacs_version` マトリクスの最古のバージョンを外し、より新しいバージョンを最小値とする
- **THEN** `README.org` のバッジもその新しい最小バージョンのマイナー系列に更新する
