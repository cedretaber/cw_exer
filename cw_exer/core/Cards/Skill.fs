namespace CardWirthEngine.Cards

open CardWirthEngine.Data
open CardWirthEngine.Data.Type
open CardWirthEngine.Scenario

module Skill =
  type t =
    { property : SkillId Property.t
    ; level : Level
    ; hold : bool
    ; motions : Motion.t list
    ; beasts : Beast.t array
    ; events : Event.t list
    }