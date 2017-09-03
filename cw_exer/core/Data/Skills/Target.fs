namespace CardWirthEngine.Data.Skills

open CardWirthEngine.Data.Types

module Target =
  type t =
    { ct : CardTarget
    ; all_range : bool
    }