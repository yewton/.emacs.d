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
#             incremental  drone だけ変わった。差分を fetch して build-fast
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
    cat borg.mk etc/borg/config.el
  } | hash_stdin
}

drones_key() {
  { git ls-tree HEAD lib/; cat .gitmodules; } | hash_stdin
}

current_state() {
  printf 'toolchain=%s\ndrones=%s\n' "$(toolchain_key)" "$(drones_key)"
}

state_field() { sed -n "s/^$1=//p" "$2"; }

cmd_key() {
  echo "toolchain=$(toolchain_key)" >> "$GITHUB_OUTPUT"
  echo "drones=$(drones_key)" >> "$GITHUB_OUTPUT"
}

cmd_plan() {
  local mode now
  now=$(mktemp)
  current_state > "$now"
  if [ "${FORCE_FULL:-}" = true ] || [ ! -f "$STATE_FILE" ]; then
    mode=full
  elif [ "$(state_field toolchain "$STATE_FILE")" != "$(state_field toolchain "$now")" ]; then
    mode=full
  elif [ "$(state_field drones "$STATE_FILE")" != "$(state_field drones "$now")" ]; then
    mode=incremental
  else
    mode=none
  fi
  rm -f "$now"
  echo "mode: $mode"
  echo "mode=$mode" >> "$GITHUB_OUTPUT"
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
  case "${1:?mode required}" in
    none) echo "drones are up to date" ;;
    incremental) make -f borg.mk build-fast ;;
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
  *) echo "usage: $0 {key|plan|sync|build MODE}" >&2; exit 2 ;;
esac
