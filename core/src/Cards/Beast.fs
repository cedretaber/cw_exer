namespace CardWirthEngine.Cards

open CardWirthEngine.Data
open CardWirthEngine.Data.Type
open CardWirthEngine.Scenario

module Beast =
  type t =
    { property : BeastId Property.t
    ; motions : Motion.t list
    ; beasts : t array
    ; events : Event.t list
    }

  let equals left right =
    Property.equals left.property right.property