namespace CardWirthEngine.Data.Types

module Enhance =
  type t
    = Action // 行動
    | Avoid // 回避
    | Resist // 抵抗
    | Defense // 防御

  type m = 
    { action : int
    ; avoid : int
    ; resist : int
    ; defense : int
    }