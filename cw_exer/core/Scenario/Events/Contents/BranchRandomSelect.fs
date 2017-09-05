namespace CardWirthEngine.Scenario.Events.Contents

open CardWirthEngine.Data.Type

module BranchRandomSelect =
  type CastRange =
    { party : bool
    ; enemy : bool
    ; npc : bool
    }

  type t =
    { range : CastRange
    ; level : LevelRange option
    ; status : Status option
    }

  let t range level status =
    { range = range
    ; level = level
    ; status = status
    }