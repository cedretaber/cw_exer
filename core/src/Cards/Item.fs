namespace CardWirthEngine.Cards

open CardWirthEngine.Data
open CardWirthEngine.Data.Type
open CardWirthEngine.Data.Types
open CardWirthEngine.Scenario

module Item =
  type t =
    { property : ItemId Property.t
    ; price : Money
    ; enhance_owner : Enhance.m
    ; hold : bool
    ; motions : Motion.t list
    ; beasts : Beast.t array
    ; events : Event.t list
    }

  let equals left right =
    Property.equals left.property right.property