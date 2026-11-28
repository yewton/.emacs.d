#!/usr/bin/env bash
# CI で Borg drone (lib/ 配下の submodule) を用意する。
#
# ワークフローは lib/ と .git/modules を actions/cache に保存しておき、
# 次回はそれを復元してから本スクリプトを呼ぶ。復元物に記録された
# 「前回ビルドしたときの状態」(STATE_FILE) と現在の状態を比べ、
# 必要な作業だけを行う。
#
#   key     キャッシュキーの材料を GITHUB_OUTPUT に書き出す
#             toolchain  Emacs 本体と Borg のビルド設定。変われば全 drone を作り直す
#             drones     各 drone の commit と .gitmodules。変われば差分ビルド
#   plan    ビルド方式を GITHUB_OUTPUT の mode に書き出す
#             none         前回と同じ。fetch もビルドも不要
#             incremental  drone だけ変わった。commit が変わった drone だけを
#                          borg-build で作り直す。.gitmodules が変わったときは
#                          build-step 等の設定変更に備えて build-fast で全 drone を回す
#             full         toolchain が変わった・復元物がない・FORCE_FULL=true。
#                          生成物を消して全 drone をビルドし直す
#   sync    submodule を index に記録された commit に揃える
#   build   mode に従ってビルドし、STATE_FILE を更新する
#
# 環境変数:
#   EMACS_MATRIX  ワークフローの matrix.emacs_version
#   FORCE_FULL    true なら常に full (定期実行でクリーンビルドを担保するため)
set -euo pipefail

STATE_FILE=.git/ci-drones-state

hash_stdin() { git hash-object --stdin; }

toolchain_key() {
  {
    emacs --batch -Q --eval '(princ (format "%s %s\n" emacs-version system-configuration))'
    # snapshot は emacs-version が同じでも中身が週次で入れ替わるので、
    # nix store のパスで実体を区別する。リリース版は同じバージョンなら
    # バイトコードの互換性があるため、パスの違い (依存の再ビルド) は無視する。
    if [ "${EMACS_MATRIX:-}" = snapshot ]; then
      readlink -f "$(command -v emacs)"
    fi
    cat borg.mk etc/borg/config.el etc/borg/build-fast.el
  } | hash_stdin
}

drones_key() {
  { git ls-tree HEAD lib/; cat .gitmodules; } | hash_stdin
}

# STATE_FILE の形式: toolchain=・gitmodules= の行に続けて git ls-tree HEAD lib/ の出力
current_state() {
  printf 'toolchain=%s\ngitmodules=%s\n' "$(toolchain_key)" "$(git hash-object .gitmodules)"
  git ls-tree HEAD lib/
}

state_field() { sed -n "s/^$1=//p" "$2"; }

# 前回から commit が変わった・新しく増えた drone の名前
changed_drones() {
  comm -13 <(grep -v = "$1" | sort) <(grep -v = "$2" | sort) |
    awk -F'\t' '{sub(/^lib\//, "", $2); print $2}'
}

cmd_key() {
  echo "toolchain=$(toolchain_key)" >> "$GITHUB_OUTPUT"
  echo "drones=$(drones_key)" >> "$GITHUB_OUTPUT"
}

cmd_plan() {
  local mode now changed=
  now=$(mktemp)
  current_state > "$now"
  if [ "${FORCE_FULL:-}" = true ] || [ ! -f "$STATE_FILE" ]; then
    mode=full
  elif [ "$(state_field toolchain "$STATE_FILE")" != "$(state_field toolchain "$now")" ]; then
    mode=full
  elif [ "$(state_field gitmodules "$STATE_FILE")" != "$(state_field gitmodules "$now")" ]; then
    mode=incremental
    changed=all
  elif ! cmp -s "$STATE_FILE" "$now"; then
    mode=incremental
    changed=$(changed_drones "$STATE_FILE" "$now" | xargs)
  else
    mode=none
  fi
  rm -f "$now"
  echo "mode: $mode${changed:+ ($changed)}"
  echo "mode=$mode" >> "$GITHUB_OUTPUT"
  echo "changed=${changed:-}" >> "$GITHUB_OUTPUT"
}

cmd_sync() {
  # 復元物には .gitmodules から消えた drone が残っていることがあるので取り除く
  local paths d
  paths=$(git config -f .gitmodules --get-regexp '\.path$' | awk '{print $2}')
  for d in lib/*/; do
    d=${d%/}
    [ -e "$d" ] || continue
    if ! grep -qxF "$d" <<<"$paths"; then
      echo "Removing stale drone: $d"
      rm -rf "$d" ".git/modules/${d#lib/}"
    fi
  done
  git submodule sync --quiet
  # 既に記録どおりの commit にある drone には触れない (ファイルの mtime を
  # 保ち、ビルド済みの .elc が古いと判定されないようにする)
  git -c protocol.version=2 submodule update --init --depth=1 --jobs 8
}

cmd_build() {
  local d
  case "${1:?mode required}" in
    none) echo "drones are up to date" ;;
    incremental)
      if [ "${2:-all}" = all ]; then
        make -f borg.mk build-fast
      else
        for d in $2; do make -f borg.mk "lib/$d"; done
      fi
      ;;
    full)
      find lib \( -name '*.elc' -o -name '*-autoloads.el' \) -delete
      make -f borg.mk build-fast
      ;;
    *) echo "unknown mode: $1" >&2; exit 1 ;;
  esac
  current_state > "$STATE_FILE"
}

case "${1:-}" in
  key) cmd_key ;;
  plan) cmd_plan ;;
  sync) cmd_sync ;;
  build) shift; cmd_build "$@" ;;
  *) echo "usage: $0 {key|plan|sync|build MODE [DRONES]}" >&2; exit 2 ;;
esac
