# Ghostel Sidebar Specification

## Purpose

ghostel 端末のバッファ名とタイトルを常設のサイドバーで一覧し、コーディングエージェントの状況確認と端末への切り替えを支援する。

## Requirements

### Requirement: Toggle a persistent terminal sidebar
システムは `toncs-ghostel-sidebar` で右サイドバーを開閉できなければならない (SHALL)。開くと一覧にフォーカスし、既存の他のパネルを維持する。

#### Scenario: Open and close
- **WHEN** コマンドを実行し、続けて再実行する
- **THEN** 右サイドバーが表示され、その後そのサイドバーだけが閉じる

### Requirement: Display buffer names and full terminal titles
一覧は全 ghostel バッファの名前とタイトルを表示しなければならない (SHALL)。タイトルは幅に応じて折り返し、未設定の場合は「タイトルなし」と表示する。

#### Scenario: Long or absent title
- **WHEN** 長いタイトルの端末とタイトル未設定の端末が存在する
- **THEN** 長いタイトルは折り返して全文を確認でき、未設定の項目には「タイトルなし」が表示される

#### Scenario: Empty list
- **WHEN** ghostel バッファが存在しない
- **THEN** サイドバーに端末がないことを示すメッセージが表示される

### Requirement: Refresh without disrupting navigation
一覧は表示中に約1秒間隔で自動更新し、`g` で即時更新できなければならない (SHALL)。既存項目の順序と選択対象を維持し、新規項目を末尾に追加する。更新でフォーカスを移してはならない (MUST NOT)。

#### Scenario: Title change and buffer creation
- **WHEN** タイトル変更、バッファ改名、端末追加が発生する
- **THEN** 次回更新で情報が反映され、既存項目の順序と選択対象が保たれる

#### Scenario: Selected buffer disappears
- **WHEN** 選択中の端末が削除される
- **THEN** 更新で項目が除去され、残る近傍項目または空一覧へ移動する

### Requirement: Select terminals in a main window
利用者は `n` / `p` で項目を移動し、`RET` または左クリックで選択した端末を通常ウィンドウに表示できなければならない (SHALL)。サイドバーは開いたまま、フォーカスは端末へ移動する。

#### Scenario: Select a live terminal
- **WHEN** 項目を確定する
- **THEN** 呼び出し元の通常ウィンドウに対応する端末が表示され、フォーカスが移る

#### Scenario: Original window is unavailable
- **WHEN** 呼び出し元が削除済みなど表示先として使用できない状態で項目を確定する
- **THEN** 同じフレームの使用可能な通常ウィンドウを使用し、候補がなければ説明メッセージを表示する

#### Scenario: Stale selection
- **WHEN** 更新前に削除された端末の項目を確定する
- **THEN** 一覧を更新して削除を知らせ、別のバッファを誤って開かない

### Requirement: Keep hidden polling lightweight and clean up timers
`q` はサイドバーを閉じなければならない (SHALL)。一覧が非表示の場合は可視性の確認だけを行い、描画や端末情報の取得を省かなければならない (SHALL)。最後の一覧ウィンドウを `q` / トグルで閉じた場合、一覧バッファの削除・モード変更・表示初期化の失敗ではタイマーを停止しなければならない (SHALL)。

#### Scenario: Hide and reopen
- **WHEN** 最後のサイドバーウィンドウを閉じ、その後再表示する
- **THEN** 明示的に閉じた後はタイマーが停止し、コマンドで再表示するとタイマーが再開する


### Requirement: Recover explicitly from refresh errors
更新中にエラーが発生した場合、システムは原因を通知して自動更新を停止しなければならない (SHALL)。利用者は手動更新または開き直しで自動更新を再開できなければならない (SHALL)。

#### Scenario: Refresh error and retry
- **WHEN** 自動更新でエラーが発生し、その後利用者が正常に手動更新する
- **THEN** エラー時にタイマーが停止し、手動更新後にタイマーが再開する

#### Scenario: Frame visibility changes
- **WHEN** 一覧を表示する最後の可視フレームを最小化し、その後復帰させる
- **THEN** 最小化中は更新を停止し、復帰後に自動更新を再開する


### Requirement: Keep terminal registration order
一覧は記録された端末の初回登録順を維持しなければならない (SHALL)。モード切り替え後も初回登録時の順序を維持しなければならない (SHALL)。

#### Scenario: Registration order differs from name order
- **WHEN** 名前順と逆の順序で端末が登録され、一覧を初期化する
- **THEN** 登録順で表示され、改名や再登録で順序が変わらない


### Requirement: Reuse an existing normal list window
一覧が同じフレームの通常ウィンドウに表示済みの場合、コマンドはそのウィンドウを選択し、一覧を更新してタイマーを再開しなければならない (SHALL)。一覧ウィンドウは端末の表示先から除外しなければならない (SHALL)。

#### Scenario: Existing normal list window
- **WHEN** 通常ウィンドウに一覧を表示してコマンドを実行する
- **THEN** そのウィンドウを選択し、一覧を追加表示せず、端末選択で一覧自身を置き換えない


### Requirement: Keep rendering separate from polling ownership
内部の再描画は停止済みタイマーを開始してはならない (MUST NOT)。表示成功後と専用の手動更新操作は、自動更新を開始できなければならない (SHALL)。

#### Scenario: Stale terminal while polling is stopped
- **WHEN** 自動更新が停止している状態で削除済み端末を選び、内部の強制再描画が行われる
- **THEN** 一覧が更新され、タイマーは停止した状態を維持する

### Requirement: Create an owned window without reusing existing panels
右側の既存パネルを維持して新しい専用ウィンドウを作成しなければならない (SHALL)。作成できない場合は既存パネルを変更せずにエラーを通知しなければならない (SHALL)。終了時に削除する対象は自身で作成した専用ウィンドウでなければならない (MUST)。

#### Scenario: Existing panel and successful open
- **WHEN** 右側のパネルが存在する状態で一覧を開いて閉じる
- **THEN** 一覧は新しいウィンドウに表示され、終了後も元のパネルの表示と設定が維持される

#### Scenario: Cannot split a new window
- **WHEN** 画面サイズまたは slot 数の上限により専用ウィンドウを作成できない
- **THEN** 作成失敗を通知し、既存パネルの表示を維持する

### Requirement: Clean up an unsuccessful open
表示処理のエラー・中断時はタイマーと作成済みの専用ウィンドウを解放しなければならない (SHALL)。

#### Scenario: Failure after creating a window
- **WHEN** 新しいサイドウィンドウの作成後に表示処理が失敗・中断する
- **THEN** 新しく作ったウィンドウが削除され、既存パネルと通常ウィンドウの内容が維持される

### Requirement: Stop polling after external window deletion
一覧が最小化フレームを含む全フレームのどのウィンドウにも表示されなくなった場合、次回タイマーで更新を停止しなければならない (SHALL)。

#### Scenario: Delete the list window externally
- **WHEN** delete-window で最後の一覧ウィンドウを削除する
- **THEN** 次回タイマーで表示先がないことを検出してタイマーが停止する

### Requirement: Configure title sampling frequency
更新間隔は toncs-ghostel-sidebar-refresh-interval で設定でき、既定は1秒でなければならない (SHALL)。正の秒数を指定し、g または開き直しで適用できなければならない (SHALL)。

#### Scenario: Sample animated titles more often
- **WHEN** 間隔を0.2秒に設定してgを実行する
- **THEN** タイマーが0.2秒間隔で動作し、その時点のタイトルを表示する

### Requirement: Keep terminal registration independent from the sidebar UI
端末の初回登録は ghostel 設定で行い、モード切り替えで記録された順序を維持しなければならない (SHALL)。端末登録はサイドバー UI をロードしてはならない (MUST NOT)。

#### Scenario: Recompile a ghostel buffer
- **WHEN** 登録済み端末が別モードへ切り替わり、ghostel-modeへ戻る
- **THEN** 別モード中は一覧から除外され、復帰後は元の登録順で表示される

#### Scenario: Open a terminal before the sidebar
- **WHEN** 起動後に端末を登録し、サイドバーをまだ開いていない
- **THEN** サイドバー UI はロードされず、永続ウィンドウパラメータ設定も変更されない


### Requirement: Reserve slot one for the sidebar
一覧は右側の slot 1 を使用しなければならない (SHALL)。slot 1 が使用済みの場合は既存ウィンドウを変更せずエラーを通知しなければならない (SHALL)。

#### Scenario: Negative-slot panel and generic right panel
- **WHEN** slot -1 の既存パネルと一覧を表示し、汎用右パネルを slot 0 に開く
- **THEN** 一覧は slot 1 に表示されたまま維持される

#### Scenario: Slot one is occupied
- **WHEN** 別のパネルが slot 1 を使用している状態で一覧を開く
- **THEN** エラーを通知し、既存パネルの表示を維持する

### Requirement: Discover terminals missed by registration
登録 hook を通らなかった ghostel バッファとその派生モードも一覧に表示しなければならない (SHALL)。

#### Scenario: A terminal existed before registration was installed
- **WHEN** 登録記録にない ghostel 派生モードのバッファが存在する
- **THEN** 更新時に発見され、共通の登録処理で末尾へ登録され、一覧に表示される


### Requirement: Append discovered terminals to one registration order
発見した未登録端末は共通の登録処理で末尾へ追加し、削除 hook を設定しなければならない (SHALL)。後から登録された端末は、既存項目に割り込んではならない (MUST NOT)。

#### Scenario: Register a terminal after discovering an unregistered terminal
- **WHEN** 未登録端末を一覧で発見した後、新しい端末を登録する
- **THEN** 新しい端末は発見済み端末の後ろに表示され、発見済み端末の削除で登録記録も除去される

### Requirement: Validate interval before side effects
更新間隔が不正な場合、表示を開く操作とgは既存のバッファ・ウィンドウ・タイマーを変更せずエラーを通知しなければならない (SHALL)。閉じる操作は実行できなければならない (SHALL)。

#### Scenario: Invalid interval before opening or manual update
- **WHEN** 更新間隔が0以下の状態で一覧を開く操作またはgを実行する
- **THEN** ウィンドウ作成や再描画・タイマー変更の前にエラーを通知する

### Requirement: Keep point unchanged for zero movement
nまたはpの移動量が0の場合は位置を変更してはならない (MUST NOT)。

#### Scenario: Zero movement outside an entry
- **WHEN** バッファ末尾で数値引数0を指定してnまたはpを実行する
- **THEN** カーソル位置が維持される


### Requirement: Cancel orphaned polling timers
一覧バッファが削除 hook を通らず消失した場合も、タイマーは次回の実行で自身を停止しなければならない (SHALL)。

#### Scenario: List buffer disappears without its kill hook
- **WHEN** 削除 hook を通さず一覧バッファを削除し、そのタイマーのコールバックを実行する
- **THEN** タイマーは自身を停止する

#### Scenario: Old callback runs after polling resumes
- **WHEN** 更新を再開した後に以前のタイマーのコールバックが実行される
- **THEN** 再開後のタイマーは維持される
