namespace CardWirthEngine.GameMasters

open CardWirthEngine.Data.Types
open CardWirthEngine.Data.Skills
open CardWirthEngine.Scenario
open State

module Standard =
  let inline message message =
    Output.Message ("", [""])

  let inline dialog dialog =
    Output.Message ("", [""])

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