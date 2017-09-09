namespace CardWirthEngine.GameMasters

namespace CardWirthEngine.GameMasters.Cards

module Input =
  type t
    (* ダイアログ・選択肢 *)
    = NextMessage of int
    | Loaded
    | LoadFailed of string
    | SelectPlayerCharactor of Adventurers.Position 
    | Cancel
    | Void
    | None
