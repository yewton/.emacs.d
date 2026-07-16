#!/usr/bin/env bash
# Epkg データベース（var/epkgs）を更新する。
#
# epkg-update そのものを batch 実行すると、内部の epkg--call-git が
# magit または switch-to-buffer-other-window を要求して成立しない。
# そこで epkg-update の処理を分解し、
#   1. pull: epkg-update が実行するのと同一の git コマンドをそのまま実行
#      （lisp/epkg.el の epkg-update 参照）
#   2. DB 再構築: epkg 自身の接続ロジック（epkg-db。既存リポジトリでは
#      batch 安全）に任せる
# とする。elisp 側への monkey patch はしない。
set -euo pipefail

EMACS_D="${EMACS_D:-$HOME/.emacs.d}"
EPKGS="$EMACS_D/var/epkgs"

usage() {
  cat >&2 <<'EOF'
Usage: update-epkgs.sh

Epkg データベース（var/epkgs、emacsmirror のパッケージ定義）を pull で
最新化し、epkg（emacs --batch）で SQLite データベースを再構築する。
lookup.sh で目当てのパッケージが見つからないときに使う。
EOF
  exit 1
}

[[ $# -eq 0 ]] || usage

if [[ ! -d "$EPKGS/.git" && ! -f "$EPKGS/.git" ]]; then
  echo "Error: Epkgs リポジトリが見つかりません: $EPKGS" >&2
  echo "初回 clone は Emacs 上で M-x epkg-list-packages 等を実行して epkg に任せてください" >&2
  exit 1
fi

echo "=== git pull (epkg-update 相当) ==="
git -C "$EPKGS" pull --no-recurse-submodules origin master

script="$(mktemp --suffix=.el)"
trap 'rm -f "$script"' EXIT

cat > "$script" <<'ELISP'
(borg-initialize)
(setq epkg-repository (expand-file-name "var/epkgs/" borg-user-emacs-directory))
(require 'epkg)
(epkg-db)
(message "Epkg データベースを再構築しました: %s" epkg-repository)
ELISP

echo
echo "=== epkg-db（SQLite 再構築） ==="
emacs -Q --batch -L "$EMACS_D/lib/borg" --load borg --load "$script"

git -C "$EPKGS" log -1 --format='最終更新: %ad (%h)' --date=short
