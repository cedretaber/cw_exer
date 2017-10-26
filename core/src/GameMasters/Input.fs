module CardWirthEngine.GameMasters.Input

open CardWirthEngine.GameMasters.Cards

type t
  (* システム系 *)
  = Loaded // ファイル読み込み成功
  | LoadFailed of string // ファイル読み込み失敗
  (* フィールド操作 *)
  | Click of int // カードクリック + カードID
  | Action
  (* ダイアログ・選択肢 *)
  | NextMessage of int // 次の選択肢 + 選択肢番号
  | SelectPlayerCharactor of Adventurers.Position // PC選択
  | Cancel // PCキャンセル
  | Void // 情報を持たない入力
  | None // 入力ではない = 前コンテンツからの継続
