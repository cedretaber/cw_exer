namespace CardWirthEngine.Cards

open CardWirthEngine.Data
open CardWirthEngine.Scenario

module rec Beast =
  type t =
    { property : Property.t
    ; motions : Motion.t
    ; beasts : Beast.t array
    ; events : Event.t list
    }