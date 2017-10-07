module CardWirthEngine.GameMasters.Scenario

open Expecto
open FsCheck

open CardWirthEngine.Data.Type
open CardWirthEngine.Scenario.BackgroundImage
open CardWirthEngine.GameMasters.Scenario

open CardWirthEngineTest.GameMasterTestUtil

let background_image =
  { smoothing = Smoothing.Default
  ; mask = false
  ; path = ""
  }

let color_cell =
  { blend_mode = BlendMode.Normal
  ; color = Color.White
  }

let text_cell =
  { text = ""
  ; font =
    { name = ""
    ; size = 14
    ; strike = false
    ; bold = false
    ; underline = false
    ; italic = false
    }
  ; vertical = false
  ; color = Color.Black
  }

let pc_cell =
  { smoothing = Smoothing.Default
  ; pc_number = 1
  }

type BackgroundImageArb () =
  static member get () =
    gen {
      let! cellname =
        Gen.oneof [ Gen.constant Option.None ; Arb.generate |> Gen.optionOf ] in
      let! location =
        Gen.choose (-1000, 1000)
        |> Gen.two
        |> Gen.map
             (function top, left -> { top = top; left = left }) in
      let! size =
        Gen.choose (0, 1000)
        |> Gen.two
        |> Gen.map
             (function height, width -> { height = height; width = width }) in
      let! flag =
        Gen.oneof [ Gen.constant Option.None; Arb.generate |> Gen.optionOf ] in
      let! level = Gen.choose (-10, 10) in
            
      let property =
        { cellname = cellname
        ; location = location
        ; size = size
        ; flag = flag
        ; level = level
        } in

      let! bg =
        [ BackgroundImage (property, background_image)
        ; ColorCell (property, color_cell)
        ; TextCell (property, text_cell)
        ; PCCell (property, pc_cell)
        ]
        |> List.map Gen.constant
        |> Gen.oneof in
        
      return bg
    } |> Arb.fromGen

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

      let config = { FsCheckConfig.defaultConfig with arbitrary = [typeof<BackgroundImageArb>] }

      yield testPropertyWithConfig config "背景を追加した場合"
        begin fun current images ->
          let scenario = { empty_scenario with backgrounds = sort_backgrounds current } in
          let scenario = add_backgrounds images scenario in
          let expected = sort_backgrounds <| images @ current in
          Expect.equal scenario.backgrounds expected "レベルの降順で並び、かつ背景継承しない部分は切り捨てられること"
        end

    ]
  ]