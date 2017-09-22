namespace CardWirthEngine.GameMasters

open CardWirthEngine.GameMasters.Cards

module Input =
  type t
    (* ダイアログ・選択肢 *)
    = NextMessage of int
    | Loaded
    | LoadFailed of string
    | SelectPlayerCharactor of Adventurers.Position 
    | Cancel
    | Void // 情報を持たない入力
    | None // 入力ではない = 前コンテンツからの継続
