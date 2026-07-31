#!/bin/bash
# lisp/*.el は org-babel-tangle の生成物なので直接編集させない（CLAUDE.md参照）。
set -euo pipefail

input="$(cat)"
file_path="$(jq -r '.tool_input.file_path // empty' <<<"$input")"

if [[ "$file_path" =~ /lisp/[^/]+\.el$ ]]; then
  jq -n '{
    hookSpecificOutput: {
      hookEventName: "PreToolUse",
      permissionDecision: "deny",
      permissionDecisionReason: "lisp/*.el は org-babel-tangle の生成物です。対応する .org ファイルを編集してください。"
    }
  }'
else
  exit 0
fi
