# project-workflow

プロジェクト単位の操作（切替・ファイル検索・grep）と、ミニバッファ補完エコシステム
（consult / marginalia / embark）との連携に関する要件。

## ADDED Requirements

### Requirement: プロジェクト操作はビルトイン project.el で提供される
プロジェクトの切替・ファイル検索・grep などの操作は、外部パッケージに依存せず
ビルトインの project.el のコマンドで提供されなければならない（MUST）。
プレフィクスキーは Emacs 標準の `C-x p`（`project-prefix-map`）を使う。

#### Scenario: プロジェクト内ファイルの検索
- **WHEN** プロジェクト内で `C-x p f` を実行する
- **THEN** `project-find-file` が起動し、プロジェクト内のファイルを補完付きで開ける

#### Scenario: 外部パッケージへの非依存
- **WHEN** Emacs を起動する
- **THEN** projectile はロードされず、`lib/projectile` は存在しない

### Requirement: プロジェクト切替直後に dired が開く
`project-switch-project`（`C-x p p`）でプロジェクトを選択したとき、
ディスパッチメニューを経由せず、選択したプロジェクトのルートで即座に
dired が開かなければならない（MUST）。

#### Scenario: 切替候補を選択する
- **WHEN** `C-x p p` で既知プロジェクトを選択する
- **THEN** ディスパッチメニューは表示されず、そのプロジェクトルートの dired バッファが開く

### Requirement: プロジェクト切替候補への embark 連携
プロジェクト切替の補完候補は embark からファイルターゲットとして扱えなければ
ならない（MUST）。特に、切替候補への `embark-act` → `v` で、そのプロジェクトの
magit-status（`embark-magit-status`）が起動できること。

#### Scenario: 切替候補から magit-status を起動する
- **WHEN** `C-x p p` の補完候補上で `embark-act`（`C-.`）を実行し `v` を押す
- **THEN** その候補のリポジトリで magit-status が開く

### Requirement: consult のプロジェクト検出は project.el に従う
consult のプロジェクト検出（`consult-project-function`）はデフォルト値のまま
project.el ベースで動作しなければならない（MUST）。独自の上書きを行わない。

#### Scenario: プロジェクト内で consult-ripgrep を使う
- **WHEN** プロジェクト内のバッファで `consult-ripgrep` をプロジェクト指定なしで実行する
- **THEN** project.el が検出したプロジェクトルートが検索対象になる
