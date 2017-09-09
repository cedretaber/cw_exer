namespace CardWirthEngine.GameMasters

open CardWirthEngine.Data
open CardWirthEngine.Data.Type
open CardWirthEngine.Data.Skills
open CardWirthEngine.Scenario
open CardWirthEngine.Scenario.Events.Contents
open CardWirthEngine.GameMasters.Cards

module Output =

  type ImageType
    = PCNumber of int
    | File of Path
    | Card

  type Message =
    { boundarycheck : bool
    ; centeringx : bool
    ; columns : int
    ; text : string
    ; selections : (int * string) list
    }

  type t'
    = StartScenario of ScenarioName
    | EndScenario of IsCompleted
    | Gameover
    | MoveArea of AreaId
    | StartBattle of BattleId
    | LoadPackage of PackageId
    | Message of message : Message // 画像なしメッセージ
    | Dialog of image : ImageType * message : Message // 画像ありメッセージ
    | Wait of Decisecond
    | Bgm of Bgm * Play.t
    | Sound of Sound * Play.t
    | Effect of target : FieldCard.t * visual :  CardVisual.t * sound : Sound
    | Flag of Flag.Name * Flag.State
    | SelectPlayerCharactor of Adventurers.Position list
    | PartyDown
    | PartyUp
    | Break
    | EventEnd
    | None

  type t = State.t * t'

  let inline t state output = state, output