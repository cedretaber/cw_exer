namespace CardWirthEngine.Scenario.Events.Contents

open CardWirthEngine.Data.Type
open CardWirthEngine.Data.Casts

module BranchAbility =
  type t =
    { level : Level
    ; target : BranchTarget
    ; physical : Physical.t
    ; mental : Mental.t
    }

  let t level target physical mental =
      { level = level
      ; target = target
      ; physical = physical
      ; mental = mental
      }