module CardWirthEngine.GameMasters.Action

open CardWirthEngine.Data.Type
open CardWirthEngine.GameMasters.Cards

type Cast
  = Cast of Adventurers.Position
  | Companion of Adventurers.Position
  | Enemy of EnemyId

type t
  (* フィールド用 *)
  = Click of int
  | UseCard of Adventurers.Position * ActionCard * int
  (* 戦闘時の行動開始 *)
  | Action of Cast
  (* 戦闘時の対象選択後効果開始 *)
  | Effect of target : Cast * actor : Cast // + 効果