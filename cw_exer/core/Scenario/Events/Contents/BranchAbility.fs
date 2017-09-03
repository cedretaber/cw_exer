namespace CardWirthEngine.Scenario.Events.Contents

open CardWirthEngine.Data.Types
open CardWirthEngine.Data.Casts

module BranchAbility =
  type t =
    { level : int
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