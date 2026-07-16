#!/usr/bin/env bash
# Borg drone (lib/ 配下の git submodule) を borg-assimilate（Emacs 経由）で
# 追加し、ビルドする。git submodule add を直接叩かない：submodule 登録・
# .gitmodules の整列・git dir の absorb・ビルドまで borg に任せるのが
# borg way であり、手作業との差分（--name の付け忘れ等）も生じない。
set -euo pipefail

EMACS_D="${EMACS_D:-$HOME/.emacs.d}"

usage() {
  cat >&2 <<'EOF'
Usage: add-drone.sh <name> <url>

  name  Borg drone 名。(require 'name) するシンボル名と一致させる
        （GitHub リポジトリ名ではない。例: akermu/emacs-libvterm -> vterm）
  url   git リポジトリの URL。lookup.sh で Epkg データベースから正規の
        URL を確認してから渡すこと

内部で emacs --batch から borg-assimilate を呼ぶ。変更（submodule 追加と
.gitmodules）はステージされた状態で残る。コミットはしない。
EOF
  exit 1
}

[[ $# -eq 2 ]] || usage
name="$1"
url="$2"

if ! [[ "$name" =~ ^[A-Za-z0-9@+_.-]+$ ]]; then
  echo "Error: drone 名に使えない文字が含まれています: ${name}" >&2
  exit 1
fi
if [[ -e "$EMACS_D/lib/${name}" ]]; then
  echo "Error: lib/${name} はすでに存在します" >&2
  exit 1
fi

script="$(mktemp --suffix=.el)"
trap 'rm -f "$script"' EXIT

cat > "$script" <<'ELISP'
(let ((name (pop command-line-args-left))
      (url  (pop command-line-args-left)))
  (borg-assimilate name url))
ELISP

emacs -Q --batch -L "$EMACS_D/lib/borg" --load borg --load "$script" "$name" "$url"

echo
echo "=== ステージされた変更 ==="
git -C "$EMACS_D" status --short -- .gitmodules "lib/${name}"
