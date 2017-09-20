namespace CardWirthEngine.GameMasters.Branch

open CardWirthEngine.GameMasters

module Random =

  let dice percent (state: State.t) =
    state.random 100 < percent

  let multi nexts (state: State.t) =
    let length = List.length nexts in
    state.random length

