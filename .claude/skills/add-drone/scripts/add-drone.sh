#!/usr/bin/env bash
# Borg drone (lib/ 配下の git submodule) を追加し、ビルドする。
set -euo pipefail

usage() {
  cat >&2 <<'EOF'
Usage: add-drone.sh <name> <url>

  name  Borg drone 名。(require 'name) するシンボル名と一致させる
        （GitHub リポジトリ名ではない。例: akermu/emacs-libvterm -> vterm）
  url   git リポジトリの URL
EOF
  exit 1
}

[[ $# -eq 2 ]] || usage
name="$1"
url="$2"
path="lib/${name}"

repo_root="$(git rev-parse --show-toplevel)"
cd "$repo_root"

if [[ -e "$path" ]]; then
  echo "Error: ${path} はすでに存在します" >&2
  exit 1
fi

git submodule add --name "$name" "$url" "$path"

echo
echo "=== make -f borg.mk build-fast ==="
build_log="$(make -f borg.mk build-fast 2>&1)"
echo "$build_log" | awk -v name="$name" '
  $0 ~ ("\\[" name "\\]") { show=1 }
  show { print }
  show && /^Done/ { exit }
'
