namespace CardWirthEngine.Event.Contents

open CardWirthEngine.Data.Types

module BranchAbility =
  type t =
    { level : int
    ; target : Target
    ; physical : Physical
    ; mental : Mental
    }

  let t level target physical mental =
      { level = level
      ; target = target
      ; physical = physical
      ; mental = mental
      }