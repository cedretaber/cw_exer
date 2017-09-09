namespace CardWirthEngine.GameMasters

module Terminal =
  let inline end_scenario is_completed =
    Output.EndScenario is_completed

  let gameover =
    Output.Gameover

  let inline move_area area_id =
    Output.MoveArea area_id

  let inline start_battle battle_id =
    Output.StartBattle battle_id

  let effect_break =
    Output.Break

  let inline is_loaded state package_id =
    true