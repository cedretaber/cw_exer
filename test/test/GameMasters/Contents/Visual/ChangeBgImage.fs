module CardWirthEngineTest.GameMasters.Contents.Visual.ChangeBgImage

open Expecto
open CardWirthEngineTest.GameMasterTestUtil

open CardWirthEngine.Scenario.Events.Content
open CardWirthEngine.GameMasters
open CardWirthEngine.GameMasters.Scenario
open CardWirthEngine.GameMasters.State
open CardWirthEngine.GameMaster
open CardWirthEngine.Data.Type
open CardWirthEngine.Scenario

[<Tests>]
let change_bg_image =
  testList "CardWirthEngine.Scenario.Events.Content.ChangeBgImage" [

    let fullsize =
      BackgroundImage.BackgroundImage
        ( { cellname = Option.None
          ; location = { left = 0; top = 0 }
          ; size = { height = 420; width = 632 }
          ; flag = Option.None
          ; level = 0
          }
        , { smoothing = Smoothing.Default
          ; mask = false
          ; path = ""
          }
        )

    let smallsize =
      BackgroundImage.BackgroundImage
        ( { cellname = Option.None
          ; location = { left = 0; top = 0 }
          ; size = { height = 100; width = 100 }
          ; flag = Option.None
          ; level = 0
          }
        , { smoothing = Smoothing.Default
          ; mask = false
          ; path = ""
          }
        )

    yield test "背景変更" {
      let images = [fullsize] in
      let ts = 5 in
      let t = Transition.Default in
      let contents = ChangeBgImage ([], ts, t, images) in
      let state = State.Scenario (empty_scenario, minimal_party, empty_global_data, state_random) in
      let state', output = read state [Content (empty_event, contents)] Input.None in
      Expect.equal output (Output.ChangeBackground (ts, t, true, false, images)) "正しく変更後の画像が返ること"
      Expect.equal (State.get_backgrounds state') (Some images) "stateの背景が更新されていること"
    }

    yield test "背景を継承する場合" {
      let current_images = [fullsize] in
      let images = [smallsize] in
      let ts = 5 in
      let t = Transition.Default in
      let contents = ChangeBgImage ([], ts, t, images) in
      let scenario = Scenario.set_backgrounds current_images empty_scenario in
      let state = State.Scenario (scenario, minimal_party, empty_global_data, state_random) in
      let state', output = read state [Content (empty_event, contents)] Input.None in
      Expect.equal output (Output.ChangeBackground (ts, t, true, false, images @ current_images)) "正しく変更後の画像が返ること"
      Expect.equal (State.get_backgrounds state') (Some <| images @ current_images) "stateの背景が更新されていること"
    }

    yield test "背景を継承しない場合（画面一杯サイズ、かつフラグなし、マスク無し）" {
      let current_images = [smallsize] in
      let images = [fullsize] in
      let ts = 5 in
      let t = Transition.Default in
      let contents = ChangeBgImage ([], ts, t, images) in
      let scenario = Scenario.set_backgrounds current_images empty_scenario in
      let state = State.Scenario (scenario, minimal_party, empty_global_data, state_random) in
      let state', output = read state [Content (empty_event, contents)] Input.None in
      Expect.equal output (Output.ChangeBackground (ts, t, true, false, images)) "正しく変更後の画像が返ること"
      Expect.equal (State.get_backgrounds state') (Some images) "stateの背景が更新されていること"
    }
  ]
