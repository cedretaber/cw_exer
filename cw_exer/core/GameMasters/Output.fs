namespace CardWirthEngine.GameMasters

open CardWirthEngine.Data
open CardWirthEngine.Data.Types

module Output =
  type t'
    = EndScenario of IsCompleted
    | Gameover
    | MoveArea of AreaId
    | StartBattle of BattleId
    | Message of text : string * selections : string list // 画像なしメッセージ
    | Dialog of path : Path * text : string * selections : string list // 画像ありメッセージ
    | Wait of decimal
    | Bgm of Bgm
    | Sound of Sound
    | Effect of PlayerPosition * Sound
    | Flag of Flag.Name * Flag.State
    | PartyDown
    | PartyUp
    | Break
    | None

  type t = State.t * t'