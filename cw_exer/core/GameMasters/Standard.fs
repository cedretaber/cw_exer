namespace CardWirthEngine.GameMasters

open CardWirthEngine.Data
open CardWirthEngine.Data.Type
open CardWirthEngine.Data.Types
open CardWirthEngine.Data.Skills
open CardWirthEngine.Scenario
open CardWirthEngine.Scenario.Events
open CardWirthEngine.Scenario.Events.Contents
open State

module Standard =

  let inline filter_selection nexts state =
    nexts
    |> List.indexed
    |> List.choose
      (function
        idx, (selection, Content.CheckFlag (_, name)) ->
          if FlagOps.get name state
          then
            Some (idx, selection)
          else
            Option.None
      | idx, (selection, Content.CheckStep (_, name, right, cmp)) ->
          let left = StepOps.get name state in
          if Comparison.compare cmp left right
          then
            Some (idx, selection)
          else
            Option.None
      | idx, (selection, _) ->
          Some (idx, selection))

  let inline message (message: TalkMessage.t) nexts state =
    let selections = filter_selection nexts state in
    Output.Message
      { boundarycheck = message.boundarycheck
      ; centeringx = message.centeringx
      ; columns = message.columns
      ; text = message.text
      ; selections = selections
      }

  let inline dialog (dialog: TalkDialog.t) nexts state =
    let selections = filter_selection nexts state in
    Output.Dialog(
      Output.Card,
      { boundarycheck = dialog.boundarycheck
      ; centeringx = dialog.centeringx
      ; columns = dialog.columns
      ; text = ""
      ; selections = selections
      })

  let inline bgm state bgm play =
    change_bgm bgm state, Output.Bgm (bgm, play)

  let inline sound sound play =
    Output.Sound (sound, play)

  let inline wait deciseconds =
    Output.Wait deciseconds

  let inline elaspe_time state =
    state

  let inline effect effect =
    Output.Effect (FieldCard.Adventurer 1, CardVisual.None, Wave "")