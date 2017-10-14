module CardWirthEngineTest.GameMasters.Contents.Visual.LoseBgImage

open Expecto
open CardWirthEngineTest.GameMasterTestUtil

open Aether
open Aether.Operators

open CardWirthEngine.Scenario.Events.Content
open CardWirthEngine.GameMasters
open CardWirthEngine.GameMasters.Scenario
open CardWirthEngine.GameMasters.State
open CardWirthEngine.GameMaster
open CardWirthEngine.Data.Type
open CardWirthEngine.Scenario

[<Tests>]
let lose_bg_image =

  let set_cellname = BackgroundImage.t.property_ >-> BackgroundImage.Property.cellname_ |> Optic.set

  testList "CardWirthEngine.Scenario.Events.Content.LoseBgImage" [
    let cellname = "cell1" in
    let image =
      BackgroundImage.BackgroundImage
        ( { cellname = Some cellname
          ; location = { left = 60; top = 60 }
          ; size = { height = 120; width = 120 }
          ; flag = Option.None
          ; level = 0
          }
        , { smoothing = Smoothing.Default
          ; mask = false
          ; path = ""
          }
        ) in
    let image0 = set_cellname (Some "cell0") image in
    let image2 = set_cellname (Some "cell2") image in
    let depiction : BackgroundImage.Depiction =
      { transition = Transition.None
      ; transition_speed = 5
      ; doanime = true
      ; ignore_effectbooster = false
      } in
    
    yield test "対象の背景が1枚の場合" {
      let images = Scenario.sort_backgrounds [image0; image; image2] in
      let scenario = Scenario.set_backgrounds images empty_scenario in
      let state = State.Scenario (scenario, minimal_party, empty_global_data, state_random) in
      let contents = LoseBgImage ([], depiction, cellname) in
      let state', output = read state [Content (empty_event, contents)] Input.None in
      let expected = [image0; image2]
      Expect.equal (State.get_backgrounds state') (Some expected) "正しく置換されること"
      Expect.equal output (Output.ChangeBackground (expected, depiction)) "正しく置換後の背景が返ること"
    }
    
    yield test "対象の背景が無い場合" {
      let images = Scenario.sort_backgrounds [image0; image2] in
      let scenario = Scenario.set_backgrounds images empty_scenario in
      let state = State.Scenario (scenario, minimal_party, empty_global_data, state_random) in
      let contents = LoseBgImage ([], depiction, cellname) in
      let state', output = read state [Content (empty_event, contents)] Input.None in
      let expected = [image0; image2]
      Expect.equal (State.get_backgrounds state') (Some expected) "正しく置換されること"
      Expect.equal output (Output.ChangeBackground (expected, depiction)) "正しく置換後の背景が返ること"
    }
    
    yield test "対象の背景が複数枚の場合" {
      let images = Scenario.sort_backgrounds [image; image0; image; image2; image] in
      let scenario = Scenario.set_backgrounds images empty_scenario in
      let state = State.Scenario (scenario, minimal_party, empty_global_data, state_random) in
      let contents = LoseBgImage ([], depiction, cellname) in
      let state', output = read state [Content (empty_event, contents)] Input.None in
      let expected = [image0; image2]
      Expect.equal (State.get_backgrounds state') (Some expected) "正しく置換されること"
      Expect.equal output (Output.ChangeBackground (expected, depiction)) "正しく置換後の背景が返ること"
    }
  ]