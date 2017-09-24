namespace CardWirthEngineTest.GameMasters.Cards

module AdventurersTest =
  open Expecto
  open CardWirthEngineTest.GameMasterTestUtil
  open CardWirthEngine.GameMasters.Cards.Adventurers

  [<Tests>]
  let adventurers =

    let empty_adv = Exist empty_cast in

    testList "CardWirthEngine.GameMasters.Cards.Adventurers" [
      testList "add" [
        test "空のパーティの冒険者を追加した場合" {
          let one_adv = [|empty_adv|] in
          let added_advs = add empty_cast no_adventurers in
          Expect.equal one_adv added_advs "パーティに追加されること"
        }

        test "既に冒険者がいるパーティに冒険者を追加した場合" {
          let two_advs = [|empty_adv; empty_adv|] in
          let added_advs = add empty_cast one_adventurer in
          Expect.equal two_advs added_advs "パーティに追加されること"
        }

        test "冒険者が6人の場合" {
          let last_adv =
            { empty_cast with 
                property = { empty_cast.property with
                               name = "last adventurer"  } } in
          let advs = [|empty_adv; empty_adv; empty_adv; empty_adv; empty_adv; Exist last_adv|] in
          let result = add empty_cast advs in
          Expect.equal result advs "元のパーティがそのまま返ること"
        }
      ]
    ]