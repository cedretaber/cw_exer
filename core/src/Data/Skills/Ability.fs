namespace CardWirthEngine.Data.Skills

open CardWirthEngine.Data.Casts

module Ability =
  type t =
    { mental : Mental.t
    ; physical : Physical.t
    }