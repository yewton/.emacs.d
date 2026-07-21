# code-navigation

## Purpose

シンボル定義へのジャンプなど、コードナビゲーション機能に関する要件。
xref を中心に、専用バックエンド（elisp・LSP 等）が存在しない場合のフォールバック手段を含む。

## Requirements

### Requirement: dumb-jump は xref のフォールバックバックエンドとして提供される
専用の xref バックエンド（elisp・LSP 等）を持たない言語での定義ジャンプを補うため、
dumb-jump は xref のフォールバックバックエンドとして登録されなければならない（MUST）。
具体的には `dumb-jump-xref-activate` が `xref-backend-functions` に登録され、
バッファローカルの専用バックエンドが優先される順序で評価されなければならない（MUST）。

#### Scenario: バックエンドが登録される
- **WHEN** Emacs を起動して設定を読み込む
- **THEN** `dumb-jump-xref-activate` が `xref-backend-functions` のグローバル値に含まれる

#### Scenario: 専用バックエンドを持たない言語で定義へ飛べる
- **WHEN** LSP 等を持たないプログラミング言語のバッファで、シンボル上にて
  `M-.`（`embark-dwim` → `xref-find-definitions`）を実行する
- **THEN** dumb-jump が定義候補を探索し、候補位置へジャンプできる

### Requirement: dumb-jump の searcher は ripgrep を使う
設定全体が ripgrep 中心であることと揃え、dumb-jump の探索には ripgrep を
使わなければならない（MUST）。git リポジトリ内でも git-grep へフォールバックせず
ripgrep を用いるため、`dumb-jump-force-searcher` を `'rg` に設定しなければならない（MUST）。

#### Scenario: 強制 searcher の設定
- **WHEN** dumb-jump がロードされ configure が実行される
- **THEN** `dumb-jump-force-searcher` の値が `rg` である

### Requirement: 既存の定義ジャンプ導線を変更しない
dumb-jump の導入はキーバインドや候補表示 UI を変更してはならない（MUST NOT）。
`M-.` は従来どおり `embark-dwim` のままとし、複数の定義候補は既存の
`xref-show-definitions-function`（`consult-xref`）を通じて表示されなければならない（MUST）。

#### Scenario: キーバインドを変更しない
- **WHEN** 設定を読み込む
- **THEN** dumb-jump 独自コマンドへのグローバルキー割り当ては追加されず、
  `M-.` は `embark-dwim` のままである

#### Scenario: 複数候補は consult 経由で表示される
- **WHEN** dumb-jump が複数の定義候補を返す
- **THEN** 候補は dumb-jump 独自のセレクタ（popup 等）ではなく `consult-xref` で提示される

### Requirement: elisp バッファでは dumb-jump が介入しない
Emacs Lisp のバッファではバッファローカルの `elisp--xref-backend` および
`embark-find-definition`（`find-function` 系）による従来のジャンプが維持され、
dumb-jump が優先されてはならない（MUST NOT）。

#### Scenario: elisp のジャンプは従来どおり
- **WHEN** Emacs Lisp バッファでシンボル上にて `M-.` を実行する
- **THEN** `embark-find-definition` ないし `elisp--xref-backend` により定義へ飛び、
  dumb-jump の探索結果には切り替わらない

### Requirement: dumb-jump は遅延ロードされる
起動時間を損なわないため、設定読み込み時に dumb-jump 本体をロードしてはならない（MUST NOT）。
autoload 済みの `dumb-jump-xref-activate` をフックに登録するにとどめ、
実際に xref を起動したときに初めて dumb-jump をロードしなければならない（MUST）。

#### Scenario: 起動直後は未ロード
- **WHEN** Emacs を起動して設定を読み込み終えた直後、まだ定義ジャンプを実行していない
- **THEN** feature `dumb-jump` はロードされていない（`featurep` が nil）
