namespace CardWirthEngine.GameMasters

open CardWirthEngine.Data
open CardWirthEngine.Data.Types

module Output =
  type t'
    = EndScenario of IsCompleted
    | Gameover
    | MoveArea of AreaId
    | StartBattle of BattleId
    | LoadPackage of PackageId
    | Message of text : string * selections : string list // 画像なしメッセージ
    | Dialog of path : Path * text : string * selections : string list // 画像ありメッセージ
    | Wait of Decisecond
    | Bgm of Bgm
    | Sound of Sound
    | Effect of int * Sound
    | Flag of Flag.Name * Flag.State
    | PartyDown
    | PartyUp
    | Break
    | EventEnd
    | None

  type t = State.t * t'