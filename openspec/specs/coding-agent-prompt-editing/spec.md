# coding-agent-prompt-editing

## Purpose

外部コーディングエージェント CLI（claude, gemini, copilot, qwen, jetski (Antigravity
CLI) 等）が `$EDITOR` 経由で開くプロンプト編集用一時ファイルに対する Emacs 側の対応
（major-mode 割り当て、popup 表示、キーバインド、ファイル参照補完）に関する要件。

## Requirements

### Requirement: Antigravity CLI の一時ファイルは markdown-mode で開かれる
Antigravity CLI がプロンプト編集のため `$EDITOR` 経由で開く一時ファイル
（ファイル名パターン `jetski-prompt-<番号>.txt`）は、拡張子 `.txt` のままでは
markdown-mode に紐付かないため、`auto-mode-alist` によって明示的に markdown-mode
が割り当てられなければならない（MUST）。

#### Scenario: jetski の一時ファイルを開く
- **WHEN** `jetski-prompt-1811438029.txt` のようなパスのファイルを開く
- **THEN** バッファは markdown-mode になる

### Requirement: Antigravity CLI の一時ファイルは popup で below に表示される
他のコーディングエージェント CLI 向け一時ファイル（gemini, copilot, claude, qwen）と
同様に、Antigravity CLI の一時ファイルも shackle により選択状態で below に popup
表示されなければならない（MUST）。

#### Scenario: jetski の一時ファイルが開かれたときの表示
- **WHEN** `jetski-prompt-<番号>.txt` パターンに一致するファイルが
  `display-buffer` される
- **THEN** 現在のウィンドウの below に popup として表示され、選択状態になる

### Requirement: Antigravity CLI の一時ファイルで汎用の保存/破棄キーバインドが使える
Antigravity CLI の一時ファイルは、他のコーディングエージェント CLI 向け一時ファイルと
同じ `kind` 判定の仕組みに乗り、汎用のキーバインドと kill-buffer 時のウィンドウ追従が
適用されなければならない（MUST）。

#### Scenario: 保存して閉じる
- **WHEN** jetski の一時ファイルのバッファで `C-c C-c` を実行する
- **THEN** バッファが保存された上で kill され、対応するポップアップウィンドウも
  閉じる

#### Scenario: 破棄して閉じる
- **WHEN** jetski の一時ファイルのバッファで `C-c C-k` を実行する
- **THEN** 変更が破棄された状態でバッファが kill され、対応するポップアップ
  ウィンドウも閉じる

### Requirement: ファイル参照補完はすべての対応エージェントで有効
`@` によるファイル参照補完（`cape-file` のトリガー文字としての `@`）は、`kind` が
判定できたすべてのコーディングエージェント CLI 一時ファイル（claude, gemini,
copilot, qwen, jetski）で有効化されなければならない（MUST）。

#### Scenario: gemini の一時ファイルで @ 補完が使える
- **WHEN** gemini の一時ファイルのバッファで `@` を入力する
- **THEN** `cape-file` によるファイル名補完候補が提示される

#### Scenario: copilot の一時ファイルで @ 補完が使える
- **WHEN** copilot の一時ファイルのバッファで `@` を入力する
- **THEN** `cape-file` によるファイル名補完候補が提示される

#### Scenario: qwen の一時ファイルで @ 補完が使える
- **WHEN** qwen の一時ファイルのバッファで `@` を入力する
- **THEN** `cape-file` によるファイル名補完候補が提示される

#### Scenario: jetski の一時ファイルで @ 補完が使える
- **WHEN** jetski の一時ファイルのバッファで `@` を入力する
- **THEN** `cape-file` によるファイル名補完候補が提示される

### Requirement: スラッシュコマンド補完は claude 専用のまま維持される
`.claude/commands` および `.claude/skills` を読み取るスラッシュコマンド補完
（`toncs-claude-command-capf`）は、`claude` の一時ファイルでのみ有効であり、
gemini・copilot・qwen・jetski では有効化してはならない（MUST NOT）。

#### Scenario: jetski の一時ファイルではスラッシュコマンド補完が出ない
- **WHEN** jetski の一時ファイルのバッファ先頭で `/` から始まる文字列を入力する
- **THEN** `toncs-claude-command-capf` による補完候補は提示されない
