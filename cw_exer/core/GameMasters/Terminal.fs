namespace CardWirthEngine.GameMasters

module Terminal =
  let inline end_scenario state is_completed =
    state, Output.EndScenario is_completed

  let inline gameover state =
    state, Output.Gameover

  let inline move_area state area_id =
    state, Output.MoveArea area_id

  let inline start_battle state battle_id =
    state, Output.StartBattle battle_id

  let inline effect_break state =
    state, Output.Break