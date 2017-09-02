namespace CardWirthEngine.GameMasters

open CardWirthEngine.Data.Types

module Input =
  type t
    (* ダイアログ・選択肢 *)
    = NextMessage of string
    | SelectAdventurer of AdventurerPosition 
    | Cancel
    | Void
    | None
