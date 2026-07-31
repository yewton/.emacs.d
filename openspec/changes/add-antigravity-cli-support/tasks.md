## 1. Antigravity CLI (jetski) 対応の追加

- [x] 1.1 `lisp/toncs-config.org` の `server` セクション、`auto-mode-alist` に
      `("/jetski-prompt-.+\\.txt\\'" . markdown-mode)` を追加する
- [x] 1.2 `toncs-server-prompt-edit-setup` 内の `cond` に
      `((string-match-p "/jetski-prompt-.+\\.txt\\'" file) 'jetski)` を追加する
- [x] 1.3 `lisp/toncs-config.org` の `shackle` セクション、`shackle-rules` に
      `("/jetski-prompt-.+\\.txt\\'" :regexp t :select t :align below :popup t)`
      を追加する

## 2. ファイル参照補完の共通化

- [x] 2.1 `toncs-server-prompt-edit-setup` 内の `(when (eq kind 'claude) ...)` を、
      `@` によるファイル参照補完（`cape-file-prefix` への `"@"` 追加）と
      スラッシュコマンド補完（`toncs-claude-command-capf` の登録）の2つに分割する
- [x] 2.2 `@` ファイル参照補完（`cape-file-prefix` への `"@"` 追加）を、`kind` が
      判定できた時点（`when-let*` 本体）で無条件に実行するよう変更する
- [x] 2.3 スラッシュコマンド補完（`toncs-claude-command-capf` の登録）は
      `(when (eq kind 'claude) ...)` のまま claude 専用に維持する

## 3. 動作確認

- [x] 3.1 `make lisp` を実行し、tangle とバイトコンパイルがエラー・警告なく
      通ることを確認する
- [x] 3.2 `make test` を実行し、既存テストが通ることを確認する
- [x] 3.3 `/tmp/jetski-prompt-<番号>.txt`相当のパスを開き、
      markdown-mode になること、below popup ルール（`:select t :align below
      :popup t`）が `shackle-rules` に登録されていること、`C-c C-c` / `C-c C-k`
      が機能すること、`@` でファイル名補完候補が出ること、スラッシュコマンド
      補完（`toncs-claude-command-capf`）が登録されないことをバッチ Emacs で
      自動検証した（`toncs-server-prompt-edit-setup` を模擬バッファに適用し、
      `cape-file-prefix` / `completion-at-point-functions` / ローカル
      キーバインドを確認）
- [x] 3.4 gemini / copilot / qwen の一時ファイルでも `@` ファイル参照補完候補が
      出るようになったことを同様にバッチ Emacs で自動検証した
