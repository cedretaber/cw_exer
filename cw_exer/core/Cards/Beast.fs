namespace CardWirthEngine.Cards

open CardWirthEngine.Data
open CardWirthEngine.Data.Type
open CardWirthEngine.Scenario

module rec Beast =
  type t =
    { property : BeastId Property.t
    ; motions : Motion.t
    ; beasts : Beast.t array
    ; events : Event.t list
    }