namespace CardWirthEngine.Cards

open CardWirthEngine.Data
open CardWirthEngine.Data.Skills
open CardWirthEngine.Scenario

module Item =
  type t =
    { property : Property.t
    ; price : int
    ; enhance_owner : Enhance.t
    ; hold : bool
    ; motions : Motion.t list
    ; beasts : Beast.t array
    ; events : Event.t list
    }