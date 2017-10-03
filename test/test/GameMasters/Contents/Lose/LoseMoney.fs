module CardWirthEngineTest.GameMasters.Contents.Lose.Money

open Expecto
open FsCheck
open CardWirthEngineTest.GameMasterTestUtil

open CardWirthEngine.Scenario.Events.Content
open CardWirthEngine.GameMasters
open CardWirthEngine.GameMasters.Scenario
open CardWirthEngine.GameMasters.State
open CardWirthEngine.GameMaster

type MoneyArb () =
  static member gen () = Arb.fromGen <| Gen.choose (0, 999999)

[<Tests>]
let get_money =
  
  let config = { FsCheckConfig.defaultConfig with arbitrary = [typeof<MoneyArb>] }

  testList "CardWirthEngine.Scenario.Events.Content.GetMoney" [
    testPropertyWithConfig config "所持金から引いた場合" <| fun balance amount ->
      let contents = LoseMoney ([], amount) in
      let party = { minimal_party with money = balance } in
      let state = State.Scenario (empty_scenario, party, empty_global_data, state_random) in
      let state', output = read state [Content (empty_event, contents)] Input.None in
      let difference =
        let diff = balance - amount in
        if diff < 0 then 0 else diff in
      Expect.equal (State.get_momey state') difference "正しく所持金が減っていること"
      Expect.equal output (Output.Money difference) "正しく所持金が返されること"
  ]