namespace CardWirthEngine.GameMasters

open CardWirthEngine.Data.Types
open State

module Standard =
  let inline message message =
    Output.Message ("", [""])

  let inline dialog dialog =
    Output.Message ("", [""])

  let inline bgm state bgm =
    { state with bgm = bgm }, Output.Bgm bgm

  let inline sound sound =
    Output.Sound sound

  let inline wait deciseconds =
    Output.Wait deciseconds

  let inline elaspe_time state =
    state

  let inline effect state effect =
    state, Output.Effect (First, Wave "")