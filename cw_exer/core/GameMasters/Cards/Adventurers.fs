namespace CardWirthEngine.GameMasters.Cards

open CardWirthEngine.Cards

module Adventurers =
  type Position
    = First
    | Second
    | Third
    | Fourth
    | Fifth
    | Sixth

  type t
    = Cast.t option
    * Cast.t option
    * Cast.t option
    * Cast.t option
    * Cast.t option
    * Cast.t option

  exception InvalidAdventurerIndexException of int

  let int_to_pos =
    function
      0 -> First
    | 1 -> Second
    | 2 -> Third
    | 3 -> Fourth
    | 4 -> Fifth
    | 5 -> Sixth
    | i -> raise <| InvalidAdventurerIndexException i

  let pos_to_int =
    function
      First  -> 0
    | Second -> 1
    | Third  -> 2
    | Fourth -> 3
    | Fifth  -> 4
    | Sixth  -> 5

  let length =
    function
      None, _, _, _, _, _ -> 0
    | _, None, _, _, _, _ -> 1
    | _, _, None, _, _, _ -> 2
    | _, _, _, None, _, _ -> 3
    | _, _, _, _, None, _ -> 4
    | _, _, _, _, _, None -> 5
    | _ -> 6

  let to_list =
    function
      a1, a2, a3, a4, a5, a6 ->
        [
          for ma in [a1; a2; a3; a4; a5; a6] do
            if Option.isSome ma then
              yield Option.get ma
        ]

  let inline get pos advs =
    match pos, advs with
      First,  (Some adv, _, _, _, _, _) -> adv
    | Second, (_, Some adv, _, _, _, _) -> adv
    | Third,  (_, _, Some adv, _, _, _) -> adv
    | Fourth, (_, _, _, Some adv, _, _) -> adv
    | Fifth,  (_, _, _, _, Some adv, _) -> adv
    | Sixth,  (_, _, _, _, _, Some adv) -> adv
    | _ -> raise <| InvalidAdventurerIndexException (pos_to_int pos)

  let inline get_by_index i =
    get <| int_to_pos i

  let inline add adv =
    function
      (a1, a2, a3, a4, a5, a6) as advs ->
        let na = Some adv in
        match advs with
          None, _, _, _, _, _ -> na, a2, a3, a4, a5 ,a6
        | _, None, _, _, _, _ -> a1, na, a3, a4, a5, a6
        | _, _, None, _, _, _ -> a1, a2, na, a4, a5, a6
        | _, _, _, None, _, _ -> a1, a2, a3, na, a5, a6
        | _, _, _, _, None, _ -> a1, a2, a3, a4, na, a6
        | _, _, _, _, _, None -> a1, a2, a3, a4, a5, na
        | _ -> advs

  let inline remove pos =
    function
      a1, a2, a3, a4, a5, a6 ->
        match pos with
          First  -> a2, a3, a4, a5, a6, None
        | Second -> a1, a3, a4, a5, a6, None
        | Third  -> a1, a2, a4, a5, a6, None
        | Fourth -> a1, a2, a3, a5, a6, None
        | Fifth  -> a1, a2, a3, a4, a6, None
        | Sixth  -> a1, a2, a3, a4, a5, None

  let rec inline remove_by_id id (advs : t) =
    let a1, a2, a3, a4, a5, a6 = advs in
    match advs with
      Some a, _, _, _, _, _ when a.property.id = id ->
        remove_by_id id (a2, a3, a4, a5 ,a6, None)
    | _, Some a, _, _, _, _ when a.property.id = id ->
        remove_by_id id (a1, a3, a4, a5 ,a6, None)
    | _, _, Some a, _, _, _ when a.property.id = id ->
        remove_by_id id (a1, a2, a4, a5 ,a6, None)
    | _, _, _, Some a, _, _ when a.property.id = id ->
        remove_by_id id (a1, a2, a3, a5 ,a6, None)
    | _, _, _, _, Some a, _ when a.property.id = id ->
        remove_by_id id (a1, a2, a3, a4 ,a6, None)
    | _, _, _, _, _, Some a when a.property.id = id ->
        remove_by_id id (a1, a2, a3, a4 ,a5, None)
    | _ -> advs

  let forall : (Cast.t -> bool) -> t -> bool =
    fun f ->
      to_list >> List.forall f

  let indexed : t -> (int * Cast.t) list =
    to_list >> List.indexed

  let contains_by : (Cast.t -> bool) -> t -> bool =
    fun f -> 
      to_list >> List.tryFind f >> Option.isSome