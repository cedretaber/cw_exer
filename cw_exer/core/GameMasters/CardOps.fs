namespace CardWirthEngine.GameMasters.Branch

open CardWirthEngine.Cards
open CardWirthEngine.GameMasters
open CardWirthEngine.GameMasters.Cards

module CardOps =
  open CardWirthEngine.GameMasters.Cards.Adventurers

  let companion_exists = State.has_companion

  let add_companion id (state : State.t) =
    let companion =
      State.get_cast id state in
    State.add_companion companion state
  
  let remove_companion = State.remove_companion

  let inline private exists fc card_in_bag count (state : State.t) =

    let check_card = fun card -> fc card >= count in

    let count_all_adventurers =
      lazy(Adventurers.fold
            (fun count cast ->
              if count <= 0
              then 0
              else count - fc cast)
            count
            state.adventurers)

    let count_backpack =
      lazy(Party.count_card card_in_bag state.party)

    function
      Selected ->
        state, Option.fold
          (fun _ -> check_card)
          false
          state.selected
    | Random ->
        match
          Adventurers.try_find_with_position
            check_card
            state.adventurers with
          None -> state, false
        | Some (pos, _) ->
            State.set_selected (State.PC pos) state, true
    | Party ->
        state, count_all_adventurers.Force () <= 0
    | Backpack ->
        state, count_backpack.Force () <= 0
    | PartyAndBackpack ->
        let rest = count - count_all_adventurers.Force ()
        state, rest <= 0 || rest - count_backpack.Force () <= 0
    | Field ->

  let add id (state : State.t) =
    let companion =
      State.get_cast id state in
    State.add_companion companion state
  
  let remove = State.remove_companion
  