module CardWirthEngineTest.GameMasters.Contents.Visual.ReplaceBgImage

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
open CardWirthEngine.Scenario.Events.Contents

[<Tests>]
let replace_bg_image =

  let set_cellname = BackgroundImage.t.property_ >-> BackgroundImage.Property.cellname_ |> Optic.set

  testList "CardWirthEngine.Scenario.Events.Content.ReplaceBgImage" [
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
    let image0 = set_cellname (Some "cell0") image in
    let image2 = set_cellname (Some "cell2") image in
    let images = Scenario.sort_backgrounds [image0; image; image2]
    let depiction : BackgroundImage.Depiction =
      { transition = Transition.None
      ; transition_speed = 5
      ; doanime = true
      ; ignore_effectbooster = false
      }

    yield test "置換対象の背景が存在しない場合" {
      let scenario = Scenario.set_backgrounds images empty_scenario in
      let state = State.Scenario (scenario, minimal_party, empty_global_data, state_random) in
      let r_image1 = image |> (fun (p : BackgroundImage.Property ) -> { p with cellname = Some "replaced cell 1" }) ^% BackgroundImage.t.property_ in
      let r_image2 = image |> (fun (p : BackgroundImage.Property ) -> { p with cellname = Some "replaced cell 2" }) ^% BackgroundImage.t.property_ in
      let contents = ReplaceBgImage ([], depiction, "no such name", [r_image1; r_image2])
      let state', output = read state [Content (empty_event, contents)] Input.None in
      let expected = images
      Expect.equal (State.get_backgrounds state') (Some expected) "正しく置換されること"
      Expect.equal output (Output.ChangeBackground (expected, depiction)) "正しく置換後の背景が返ること"
    }

    yield test "置換対象の背景が1つだけある場合" {
      let scenario = Scenario.set_backgrounds images empty_scenario in
      let state = State.Scenario (scenario, minimal_party, empty_global_data, state_random) in
      let r_image = image |> (fun (p : BackgroundImage.Property ) -> { p with cellname = Some "replaced cell 1" }) ^% BackgroundImage.t.property_ in
      let contents = ReplaceBgImage ([], depiction, cellname, [r_image])
      let state', output = read state [Content (empty_event, contents)] Input.None in
      let expected = Scenario.sort_backgrounds [image0; r_image; image2]
      Expect.equal (State.get_backgrounds state') (Some expected) "正しく置換されること"
      Expect.equal output (Output.ChangeBackground (expected, depiction)) "正しく置換後の背景が返ること"
    }

    yield test "置換対象の背景が複数ある場合" {
      let scenario = Scenario.set_backgrounds images empty_scenario in
      let state = State.Scenario (scenario, minimal_party, empty_global_data, state_random) in
      let r_image1 = image |> (fun (p : BackgroundImage.Property ) -> { p with cellname = Some "replaced cell 1" }) ^% BackgroundImage.t.property_ in
      let r_image2 = image |> (fun (p : BackgroundImage.Property ) -> { p with cellname = Some "replaced cell 2" }) ^% BackgroundImage.t.property_ in
      let contents = ReplaceBgImage ([], depiction, cellname, [r_image1; r_image2])
      let state', output = read state [Content (empty_event, contents)] Input.None in
      let expected = Scenario.sort_backgrounds [image0; r_image1; r_image2; image2]
      Expect.equal (State.get_backgrounds state') (Some expected) "正しく置換されること"
      Expect.equal output (Output.ChangeBackground (expected, depiction)) "正しく置換後の背景が返ること"
    }
  ]