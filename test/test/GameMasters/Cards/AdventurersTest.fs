namespace CardWirthEngineTest.GameMasters.Cards

open NUnit.Framework

open CardWirthEngineTest.TestUtils
open CardWirthEngineTest.GameMasterTestUtil

module AdventurersTest =
  open CardWirthEngine.GameMasters.Cards.Adventurers

  module add =

    let empty_adv = Exist empty_cast in

    [<Test>]
    let ``空のパーティの冒険者を追加した場合`` () =
      let one_adv =
        empty_adv, Nothing, Nothing, Nothing, Nothing, Nothing in
      add empty_cast no_adventurers === one_adv

    [<Test>]
    let ``既に冒険者がいるパーティに冒険者を追加した場合`` () =
      let two_advs =
        empty_adv, empty_adv, Nothing, Nothing, Nothing, Nothing in
      add empty_cast one_adventurer === two_advs

    [<Test>]
    let ``冒険者が6人の場合、元のパーティがそのまま返ること`` () =
      let last_adv =
        { empty_cast with 
            property = { empty_cast.property with
                           name = "last adventurer"  } } in
      let advs =
        empty_adv, empty_adv, empty_adv, empty_adv, empty_adv, Exist last_adv in
      let _, _, _, _, _, result = add empty_cast advs in
      result === Exist last_adv