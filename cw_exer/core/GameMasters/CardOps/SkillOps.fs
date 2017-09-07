namespace CardWirthEngine.GameMasters.Cards

open CardWirthEngine.GameMasters

module CastOps =
  let exists = State.has_companion

  let add id (state : State.t) =
    let companion =
      State.get_cast id state in
    State.add_companion companion state
  
  let remove = State.remove_companion
  