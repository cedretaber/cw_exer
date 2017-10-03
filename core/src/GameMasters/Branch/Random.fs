module CardWirthEngine.GameMasters.Branch.Random

open CardWirthEngine.GameMasters

let dice percent state =
  State.random 100 state < percent

let multi nexts =
  State.random <| List.length nexts

