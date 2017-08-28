﻿namespace CardWirthEngine.Scenario.Event.Contents

open CardWirthEngine.Data.Types

module BranchRandomSelect =
  type CastRange
    = Party // パーティ全体
    | Enemy // 敵全体
    | Npc // 同行キャスト全体

  type t = {
    range : CastRange list;
    level : int option;
    status : Status option
  }

  let t range level status =
    { range = range; level = level; status = status }