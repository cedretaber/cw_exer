namespace CardWirthEngine.GameMasters.Cards

open CardWirthEngine.GameMasters

module CastOps =
  let add id (state : State.t) =
    let companion =
      State.get_cast id state in
    State.add_companion companion state

  