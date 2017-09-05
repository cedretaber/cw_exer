namespace CardWirthEngine.Data.Skills

open CardWirthEngine.Data.Type

module Target =
  type t =
    { ct : CardTarget
    ; all_range : bool
    }