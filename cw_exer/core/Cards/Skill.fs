namespace CardWirthEngine.Cards

open CardWirthEngine.Data
open CardWirthEngine.Scenario

module Skill =
    
  type t =
    { property : Property.t
    ; level : int
    ; hold : bool
    ; motions : Motion.t list
    ; beasts : Beast.t array
    ; events : Event.t list
    }