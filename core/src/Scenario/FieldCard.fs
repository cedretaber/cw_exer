namespace CardWirthEngine.Scenario

module FieldCard =
  type t
    = Adventurer of int
    | Companion of int
    | MenuCard of int
    | Enemy of int