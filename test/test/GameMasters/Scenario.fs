module CardWirthEngine.GameMasters.Scenario

open Expecto
open FsCheck

open CardWirthEngine.Data.Type
open CardWirthEngine.Scenario.BackgroundImage
open CardWirthEngine.GameMasters.Scenario

open CardWirthEngineTest.GameMasterTestUtil

[<Tests>]
let scenario_test =
  testList "CardWirthEngine.GameMasters.Scenario" [
    testList "add_backgrounds" [
      let full_property =
        { cellname = Option.None
        ; location = { top = 0; left = 0 }
        ; size = { height = 420; width = 632 }
        ; flag = Option.None
        ; level = 0
        }
      let background_image =
        { smoothing = Smoothing.Default
        ; mask = false
        ; path = ""
        }

      let full_image = BackgroundImage (full_property, background_image)

      let property = { full_property with size = { height = 100; width = 100 } }

      let image = BackgroundImage (property, background_image)

      yield test "空の背景に背景を足した場合" {
        let scenario = add_backgrounds [full_image] empty_scenario in
        Expect.equal scenario.backgrounds [full_image] "背景が追加されていること"
      }

      yield test "既に背景がある場合" {
        let scenario = { empty_scenario with backgrounds = [full_image] } in
        let scenario = add_backgrounds [image] scenario in
        Expect.equal scenario.backgrounds [image; full_image] "背景が前に追加されていること"
      }

      yield test "追加される背景が背景を継承しない場合" {
        let scenario = { empty_scenario with backgrounds = [image] } in
        let scenario = add_backgrounds [full_image] scenario in
        Expect.equal scenario.backgrounds [full_image] "継承されない部分は残っていないこと"
      }
    ]
  ]