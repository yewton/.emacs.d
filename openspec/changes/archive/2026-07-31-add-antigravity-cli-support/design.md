## Context

`lisp/toncs-config.org` の `server` セクションと `shackle` セクションには、
コーディングエージェント CLI（gemini, copilot, claude, qwen）が `$EDITOR` 経由で
開くプロンプト編集用一時ファイルに対する設定が、CLI ごとに次の3箇所へ同じパターンで
書かれている。

1. `auto-mode-alist`（拡張子なし/`.txt` のファイルに markdown-mode を明示割り当て）
2. `toncs-server-prompt-edit-setup` 内の `cond`（ファイルパスの正規表現から `kind`
   を判定し、`kind` に応じてキーバインドや補完を設定）
3. `shackle-rules`（popup 表示ルール）

`@` によるファイル参照補完（`cape-file-prefix` への `"@"` 追加）は、現状
`(eq kind 'claude)` の中でスラッシュコマンド補完と一緒くたに有効化されている。

Google の Antigravity CLI は内部コードネーム "jetski" の名残で、プロンプト編集用
一時ファイルを `jetski-prompt-<番号>.txt`（拡張子 `.txt`、サブディレクトリなし）
という名前で `/tmp` 直下に作成する。`Ctrl+G` で `$EDITOR` を起動する点、複数行の
プロンプトを編集する点は既存の4エージェントと同じであり、`@` によるファイル参照と
スラッシュコマンドの両方を持つが、スラッシュコマンドは仕様が Antigravity 固有で
保守コストが見合わないため今回は対応しない。

## Goals / Non-Goals

**Goals:**
- Antigravity CLI (jetski) の一時ファイルを他の4エージェントと同じ体験（markdown-mode
  + popup below + 汎用キーバインド）で開けるようにする
- `@` ファイル参照補完を claude 専用の実装から切り離し、`kind` が判定できる全エージェント
  （claude, gemini, copilot, qwen, jetski）で使えるようにする

**Non-Goals:**
- Antigravity CLI のスラッシュコマンド補完への対応（`.claude/commands` 相当の仕組みが
  Antigravity にどう対応するか未調査であり、対応しない）
- 既存4エージェントのキーバインドや表示位置など、ファイル参照補完以外の挙動変更

## Decisions

### jetski の正規表現マッチングは既存パターンを踏襲する

`jetski-prompt-<番号>.txt` は claude（フラット配置 + 拡張子固定）と gemini/qwen
（`.txt` 拡張子で markdown-mode を明示指定する必要がある点）のハイブリッドである。
既存の正規表現はいずれもパス末尾へのマッチであり、ディレクトリ構造の違いは影響しない
ため、`"/jetski-prompt-.+\\.txt\\'"` という単純な正規表現を `auto-mode-alist`・
`cond`・`shackle-rules` の3箇所にそのまま追加する。番号部分（PID/タイムスタンプ
と推測されるが未確定）は `.+` で吸収し、具体的な採番方式には依存しない。

**代替案**: 番号部分を `[0-9]+` に限定する案も検討したが、実際の採番規則が
ドキュメント上確認できていないため、他の4パターンと同様に `.+` で緩く受ける
方針を採る。

### `@` ファイル参照補完は `kind` が判定できた時点で無条件に有効化する

当初は `claude` 専用だった `(setq-local cape-file-prefix (cons "@" ...))` を、
`gemini`・`copilot`・`qwen`・`jetski` 全てに広げる必要が生じた。5つの `kind` すべてで
有効化する方針が確定したため、`(when (memq kind '(...)) ...)` のような許可リストを
作らず、`when-let*` 本体（`kind` が非 nil で確定した時点）で無条件に実行する形に
単純化する。スラッシュコマンド補完（`toncs-claude-command-capf`）だけを
引き続き `(when (eq kind 'claude) ...)` で claude 限定に保つ。

**代替案**: `kind` ごとに `@` 補完の要否を管理する連想リストを作る案も考えたが、
現時点で全 `kind` が対象であり、将来「`@` 補完を持たない CLI」が増えたときに
初めて分岐を戻せばよいため、過剰な抽象化を避けて条件を削除する方をとる。

## Risks / Trade-offs

- [Risk] Antigravity CLI の一時ファイル命名規則（番号部分の意味、ディレクトリが
  常に `/tmp` 直下か）がユーザー提供の一例のみに基づいており、将来 Antigravity
  側の実装変更でパターンが変わる可能性がある
  → Mitigation: 他の4エージェントも同様にパス末尾の緩いマッチに依存しており、
  リスクの性質は既存パターンと同等。ズレが判明した時点で正規表現を調整すればよい
- [Risk] `@` 補完が今まで有効でなかった gemini/copilot/qwen で、CLI 側の実際の
  ファイル参照構文が `@` と一致しない場合、補完が誤作動する可能性がある
  → Mitigation: ユーザーが実際の利用経験から「ファイル参照がある」ことを確認済み
  であり、`cape-file-prefix` への追加はトリガー文字を増やすだけで既存の `@`
  なし入力には影響しない

## Migration Plan

設定ファイルの追記のみでバックエンド互換性の問題はない。`make lisp` で
tangle・バイトコンパイルし、`make test` で既存テストが通ることを確認する。
ロールバックは該当コミットの revert で足りる。

## Open Questions

- jetski の一時ファイルの番号部分の正確な意味（PID か timestamp か）は未確認だが、
  実装上は影響しない
