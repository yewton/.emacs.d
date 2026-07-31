#!/usr/bin/env bash
# assimilate 後にビルドや設定で失敗し、コミット前に最初からやり直したい場合の
# クリーンアップ。lib/<name> submodule の登録解除・削除と .gitmodules の復元を
# まとめて行う。
set -euo pipefail

EMACS_D="${EMACS_D:-$HOME/.emacs.d}"

usage() {
  cat >&2 <<'EOF'
Usage: abort.sh <name>

  name  add-drone.sh で追加した drone 名（lib/<name>）

assimilate 後、コミット前の状態を対象に lib/<name> submodule の登録解除・
削除、.git/modules 配下の後始末、.gitmodules の復元を行う。コミット済みの
drone には使わないこと（git checkout -- .gitmodules が他の変更も巻き戻す）。
EOF
  exit 1
}

[[ $# -eq 1 ]] || usage
name="$1"

if ! [[ "$name" =~ ^[A-Za-z0-9@+_.-]+$ ]]; then
  echo "Error: drone 名に使えない文字が含まれています: ${name}" >&2
  exit 1
fi
if [[ ! -e "$EMACS_D/lib/${name}" ]]; then
  echo "Error: lib/${name} が見つかりません" >&2
  exit 1
fi

git -C "$EMACS_D" submodule deinit -f "lib/${name}"
git -C "$EMACS_D" rm -f "lib/${name}"
rm -rf "$EMACS_D/.git/modules/lib/${name}" "$EMACS_D/.git/modules/${name}"
git -C "$EMACS_D" checkout -- .gitmodules

echo
echo "=== 現在の状態 ==="
git -C "$EMACS_D" status --short -- .gitmodules
