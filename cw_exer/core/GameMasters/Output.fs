namespace CardWirthEngine.GameMasters

open CardWirthEngine.Data
open CardWirthEngine.Data.Types
open CardWirthEngine.Data.Skills
open CardWirthEngine.Scenario

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
    | Effect of target : FieldCard.t * visual :  CardVisual.t * sound : Sound
    | Flag of Flag.Name * Flag.State
    | PartyDown
    | PartyUp
    | Break
    | EventEnd
    | None

  type t = State.t * t'