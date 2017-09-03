namespace CardWirthEngine.GameMasters

open CardWirthEngine.Data.Types

module Input =
  type t
    (* ダイアログ・選択肢 *)
    = NextMessage of int
    | Loaded
    | LoadFailed of string
    | SelectPlayerCharactor of int 
    | Cancel
    | Void
    | None
