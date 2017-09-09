namespace CardWirthEngine.Scenario.Events.Contents

open CardWirthEngine.Data.Type

module BranchKeyCode =
  type CardType
    = All // 全種類
    | Skill // 特殊技能
    | Item // アイテム
    | Beast // 召喚獣
    | Hand // 手札(Wsn.2)

  type t =
    { range : Range
    ; card_type : CardType
    ; key_code : KeyCode
    }

  let t range card_type key_code =
    { range = range; card_type = card_type; key_code = key_code }