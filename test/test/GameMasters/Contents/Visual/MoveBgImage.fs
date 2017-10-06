module CardWirthEngineTest.GameMasters.Contents.Visual.MoveBgImage

open Expecto
open CardWirthEngineTest.GameMasterTestUtil

open CardWirthEngine.Scenario.Events.Content
open CardWirthEngine.GameMasters
open CardWirthEngine.GameMasters.Scenario
open CardWirthEngine.GameMasters.State
open CardWirthEngine.GameMaster
open CardWirthEngine.Data.Type
open CardWirthEngine.Scenario
open CardWirthEngine.Scenario.Events.Contents

[<Tests>]
let move_bg_image =
  testList "CardWirthEngine.Scenario.Events.Content.MoveBgImage" [
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
        )
    let scenario = Scenario.set_backgrounds [image] empty_scenario in
    let state = State.Scenario (scenario, minimal_party, empty_global_data, state_random) in
    let ts = 5 in
    let t = Transition.None in
    let doanime = true in
    let ie = false in

    yield testList "サイズの変更" begin
            let resize coordinate_type : MoveBackgroundImage.t =
              { cellname = cellname
              ; position = { coordinate_type = CoordinateType.None; x = 0; y = 0 }
              ; size = { coordinate_type = coordinate_type; width = 50; height = 50 }
              ; transition = t
              ; transition_speed = ts
              ; doanime = doanime
              ; ignore_effectbooster = ie
              }

            testParam resize [
              "絶対サイズ"
              , fun resize () ->
                  let resize = resize CoordinateType.Absolute in
                  let contents = MoveBgImage ([], resize) in
                  let state', output = read state [Content (empty_event, contents)] Input.None in
                  let expected =
                    BackgroundImage.map_size
                      (fun size -> { size with width = 50; height = 50 })
                      image in
                  Expect.equal output (Output.ChangeBackground (ts, t, doanime, ie, [expected])) "正しく変更後の画像が返ること"
                  Expect.equal (State.get_backgrounds state') (Some [expected]) "正しく背景が変化していること"
              "相対サイズ"
              , fun resize () ->
                  let resize = resize CoordinateType.Relative in
                  let contents = MoveBgImage ([], resize) in
                  let state', output = read state [Content (empty_event, contents)] Input.None in
                  let expected =
                    BackgroundImage.map_size
                      (fun size -> { size with width = size.width + 50; height = size.height + 50 })
                      image in
                  Expect.equal output (Output.ChangeBackground (ts, t, doanime, ie, [expected])) "正しく変更後の画像が返ること"
                  Expect.equal (State.get_backgrounds state') (Some [expected]) "正しく背景が変化していること"
              "割合"
              , fun resize () ->
                  let resize = resize CoordinateType.Percentage in
                  let contents = MoveBgImage ([], resize) in
                  let state', output = read state [Content (empty_event, contents)] Input.None in
                  let expected =
                    BackgroundImage.map_size
                      (fun size -> { size with width = 60; height = 60 }) (* 120の50% = 60 *)
                      image in
                  Expect.equal output (Output.ChangeBackground (ts, t, doanime, ie, [expected])) "正しく変更後の画像が返ること"
                  Expect.equal (State.get_backgrounds state') (Some [expected]) "正しく背景が変化していること"
            ] |> List.ofSeq
          end

    yield testList "位置の変更" begin
            let move coordinate_type : MoveBackgroundImage.t =
              { cellname = cellname
              ; position = { coordinate_type = coordinate_type; x = 50; y = 50 }
              ; size = { coordinate_type = CoordinateType.None; width = 0; height = 0 }
              ; transition = t
              ; transition_speed = ts
              ; doanime = doanime
              ; ignore_effectbooster = ie
              }

            testParam move [
              "絶対サイズ"
              , fun move () ->
                  let move = move CoordinateType.Absolute in
                  let contents = MoveBgImage ([], move) in
                  let state', output = read state [Content (empty_event, contents)] Input.None in
                  let expected =
                    BackgroundImage.map_location
                      (fun location -> { location with top = 50; left = 50 })
                      image in
                  Expect.equal output (Output.ChangeBackground (ts, t, doanime, ie, [expected])) "正しく変更後の画像が返ること"
                  Expect.equal (State.get_backgrounds state') (Some [expected]) "正しく背景が移動していること"
              "相対サイズ"
              , fun move () ->
                  let move = move CoordinateType.Relative in
                  let contents = MoveBgImage ([], move) in
                  let state', output = read state [Content (empty_event, contents)] Input.None in
                  let expected =
                    BackgroundImage.map_location
                      (fun location -> { location with top = location.top + 50; left = location.left + 50 })
                      image in
                  Expect.equal output (Output.ChangeBackground (ts, t, doanime, ie, [expected])) "正しく変更後の画像が返ること"
                  Expect.equal (State.get_backgrounds state') (Some [expected]) "正しく背景が移動していること"
              "割合"
              , fun move () ->
                  let move = move CoordinateType.Percentage in
                  let contents = MoveBgImage ([], move) in
                  let state', output = read state [Content (empty_event, contents)] Input.None in
                  let expected =
                    BackgroundImage.map_location
                      (fun location -> { location with top = 30; left = 30 }) (* 60の50% = 30 *)
                      image in
                  Expect.equal output (Output.ChangeBackground (ts, t, doanime, ie, [expected])) "正しく変更後の画像が返ること"
                  Expect.equal (State.get_backgrounds state') (Some [expected]) "正しく背景が移動していること"
            ] |> List.ofSeq
          end

    // TODO: 変更が無い場合（cellnameが一致しない）場合のテスト、複数個の画像が変化する場合のテスト
  ]