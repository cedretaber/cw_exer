namespace CardWirthEngine.GameMasters.Branch

open CardWirthEngine.Data.Type
open CardWirthEngine.Cards
open CardWirthEngine.Scenario.Events.Contents
open CardWirthEngine.GameMasters
open CardWirthEngine.GameMasters.Cards

module KeyCode =

  module BKC = BranchKeyCode

  let has_key_code range card_type key_code (state : State.t) =
    let (skills, items, beasts, hands) =
      match card_type with
        BKC.All -> true, true, true, false
      | BKC.Skill -> true, false, false, false
      | BKC.Item -> false, true, false, false
      | BKC.Beast -> false, false, false, false
      | BKC.Hand -> false, false, false, true in

    let target (cast : Cast.t) =
      seq {
        if skills then
          for skill in cast.skill do
            for kc in skill.property.key_codes -> kc
        if items then
          for item in cast.item do
            for kc in item.property.key_codes -> kc
        if beasts then
          for beast in cast.beast do
            for kc in beast.property.key_codes -> kc
        // TODO: 手札未実装
      } in

    let f cast =
      Seq.tryFind
        ((=) key_code)
        (target cast)
      |> Option.isSome
    let f' (card : Adventurers.CardState) = f card.cast

    match range with
      Range.Selected ->
        match state.selected_cast with
          Some cast -> state, f cast
        | Option.None -> state, false
    | Range.Party ->
        state, Adventurers.forall f' (State.get_adventurers state)
    // TODO: その他未実装
    | _ ->
        state, false