## Why

コーディングエージェント CLI（gemini, copilot, claude, qwen）が `$EDITOR` 経由で開く
プロンプト編集用一時ファイルは、専用の major-mode 割り当てと popup 表示、キーバインドが
`toncs-config.org` に用意されている。Google の Antigravity CLI も同様に `$EDITOR` を
起動してプロンプトを編集する機能を持つが、この設定には未対応。また既存実装では
`@` によるファイル参照補完が `claude` 専用になっており、他の CLI でも同機能が使える
にもかかわらず有効化されていない。

## What Changes

- Antigravity CLI（一時ファイル名は `jetski-prompt-<番号>.txt`）のプロンプト編集用
  一時ファイルに対応する
  - `auto-mode-alist` へ markdown-mode を割り当てるエントリを追加
  - `shackle-rules` へ popup 表示ルールを追加
  - `toncs-server-prompt-edit-setup` の `kind` 判定に `jetski` を追加し、既存の
    汎用キーバインド（`C-c C-c` / `C-c C-k` 等）を適用する
- `@` によるファイル参照補完（`cape-file-prefix` への `@` 追加）を `claude` 専用から
  `kind` が判定できた全エージェント（claude, gemini, copilot, qwen, jetski）共通の
  挙動に変更する
- スラッシュコマンド補完（`.claude/commands` / `.claude/skills` を読む
  `toncs-claude-command-capf`）は `claude` 専用のまま維持する（Antigravity CLI にも
  スラッシュコマンドはあるが、保守コストが見合わないため今回は対応しない）

## Capabilities

### New Capabilities
- `coding-agent-prompt-editing`: 外部コーディングエージェント CLI が `$EDITOR` 経由で
  開くプロンプト編集用一時ファイルに対する Emacs 側の対応（major-mode 割り当て、popup
  表示、キーバインド、ファイル参照補完）に関する要件

### Modified Capabilities
(なし。既存の spec に対応する capability は存在しない)

## Impact

- 影響ファイル: `lisp/toncs-config.org`（`server` セクション、`shackle` セクション）
- 既存の gemini / copilot / claude / qwen 向け設定の挙動は、ファイル参照補完が
  新たに有効になる点を除き変更しない
