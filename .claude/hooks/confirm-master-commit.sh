#!/bin/bash
# OpenSpec change（/opsx:*）の実装はmasterへ直接コミットせずブランチで作業する
# というCLAUDE.mdの指示がprose頼みだったため、masterでのgit commitには確認を挟む。
set -euo pipefail

input="$(cat)"
command="$(jq -r '.tool_input.command // empty' <<<"$input")"
cwd="$(jq -r '.cwd // "."' <<<"$input")"

if [[ "$command" =~ git\ (-C\ ([^\ ]+)\ )?commit ]]; then
  target="${BASH_REMATCH[2]:-$cwd}"
  branch="$(git -C "$target" rev-parse --abbrev-ref HEAD 2>/dev/null || true)"
  if [[ "$branch" == "master" ]]; then
    jq -n '{
      hookSpecificOutput: {
        hookEventName: "PreToolUse",
        permissionDecision: "ask",
        permissionDecisionReason: "masterブランチへの直接コミットです。OpenSpec changeの実装であればchange名を含むブランチを切ってください。"
      }
    }'
    exit 0
  fi
fi
exit 0
