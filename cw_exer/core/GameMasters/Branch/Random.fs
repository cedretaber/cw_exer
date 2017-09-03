namespace CardWirthEngine.GameMasters.Branch

open CardWirthEngine.GameMasters

module Random =
  let dice percent (state: State.t) =
    state.random 100 > percent

