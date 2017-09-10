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

  let positions =
    [ First
    ; Second
    ; Third
    ; Fourth
    ; Fifth
    ; Sixth
    ]

  type CardState
    = Exist of Cast.t
    | Flipped of Cast.t
    | Nothing

  type t
    = CardState
    * CardState
    * CardState
    * CardState
    * CardState
    * CardState

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
      Nothing, _, _, _, _, _ -> 0
    | _, Nothing, _, _, _, _ -> 1
    | _, _, Nothing, _, _, _ -> 2
    | _, _, _, Nothing, _, _ -> 3
    | _, _, _, _, Nothing, _ -> 4
    | _, _, _, _, _, Nothing -> 5
    | _ -> 6

  let inline to_list'' include_flipped =
    function
      a1, a2, a3, a4, a5, a6 ->
        [
          for ma in [a1; a2; a3; a4; a5; a6] do
            match ma with
              Exist a -> yield a
            | Flipped a when include_flipped -> yield a
            | _ -> ()
        ]

  let to_list = to_list'' false
  let to_list' = to_list'' true

  let inline to_seq'' include_flipped =
    function
      a1, a2, a3, a4, a5, a6 ->
        seq {
          for ma in [a1; a2; a3; a4; a5; a6] do
            match ma with
              Exist a -> yield a
            | Flipped a when include_flipped -> yield a
            | _ -> ()
        }

  let to_seq = to_seq'' false
  let to_seq' = to_seq'' true

  let to_seq_with_pos = to_seq >> Seq.zip positions

  let inline get pos advs =
    match pos, advs with
      First,  (Exist adv, _, _, _, _, _) -> adv
    | Second, (_, Exist adv, _, _, _, _) -> adv
    | Third,  (_, _, Exist adv, _, _, _) -> adv
    | Fourth, (_, _, _, Exist adv, _, _) -> adv
    | Fifth,  (_, _, _, _, Exist adv, _) -> adv
    | Sixth,  (_, _, _, _, _, Exist adv) -> adv
    | First,  (Flipped adv, _, _, _, _, _) -> adv
    | Second, (_, Flipped adv, _, _, _, _) -> adv
    | Third,  (_, _, Flipped adv, _, _, _) -> adv
    | Fourth, (_, _, _, Flipped adv, _, _) -> adv
    | Fifth,  (_, _, _, _, Flipped adv, _) -> adv
    | Sixth,  (_, _, _, _, _, Flipped adv) -> adv
    | _ -> raise <| InvalidAdventurerIndexException (pos_to_int pos)

  let inline get_by_index i =
    get <| int_to_pos i

  let inline add adv =
    function
      (a1, a2, a3, a4, a5, a6) as advs ->
        let na = Exist adv in
        match advs with
          Nothing, _, _, _, _, _ -> na, a2, a3, a4, a5 ,a6
        | _, Nothing, _, _, _, _ -> a1, na, a3, a4, a5, a6
        | _, _, Nothing, _, _, _ -> a1, a2, na, a4, a5, a6
        | _, _, _, Nothing, _, _ -> a1, a2, a3, na, a5, a6
        | _, _, _, _, Nothing, _ -> a1, a2, a3, a4, na, a6
        | _, _, _, _, _, Nothing -> a1, a2, a3, a4, a5, na
        | _ -> advs

  let inline remove pos =
    function
      a1, a2, a3, a4, a5, a6 ->
        match pos with
          First  -> a2, a3, a4, a5, a6, Nothing
        | Second -> a1, a3, a4, a5, a6, Nothing
        | Third  -> a1, a2, a4, a5, a6, Nothing
        | Fourth -> a1, a2, a3, a5, a6, Nothing
        | Fifth  -> a1, a2, a3, a4, a6, Nothing
        | Sixth  -> a1, a2, a3, a4, a5, Nothing

  let rec remove_by_id id =
    function
      Exist a, a2, a3, a4, a5, a6 when a.property.id = id ->
        remove_by_id id (a2, a3, a4, a5 ,a6, Nothing)
    | a1, Exist a, a3, a4, a5, a6 when a.property.id = id ->
        remove_by_id id (a1, a3, a4, a5 ,a6, Nothing)
    | a1, a2, Exist a, a4, a5, a6 when a.property.id = id ->
        remove_by_id id (a1, a2, a4, a5 ,a6, Nothing)
    | a1, a2, a3, Exist a, a5, a6 when a.property.id = id ->
        remove_by_id id (a1, a2, a3, a5 ,a6, Nothing)
    | a1, a2, a3, a4, Exist a, a6 when a.property.id = id ->
        remove_by_id id (a1, a2, a3, a4 ,a6, Nothing)
    | a1, a2, a3, a4, a5, Exist a when a.property.id = id ->
        remove_by_id id (a1, a2, a3, a4 ,a5, Nothing)
    | Flipped a, a2, a3, a4, a5, a6 when a.property.id = id ->
        remove_by_id id (a2, a3, a4, a5 ,a6, Nothing)
    | a1, Flipped a, a3, a4, a5, a6 when a.property.id = id ->
        remove_by_id id (a1, a3, a4, a5 ,a6, Nothing)
    | a1, a2, Flipped a, a4, a5, a6 when a.property.id = id ->
        remove_by_id id (a1, a2, a4, a5 ,a6, Nothing)
    | a1, a2, a3, Flipped a, a5, a6 when a.property.id = id ->
        remove_by_id id (a1, a2, a3, a5 ,a6, Nothing)
    | a1, a2, a3, a4, Flipped a, a6 when a.property.id = id ->
        remove_by_id id (a1, a2, a3, a4 ,a6, Nothing)
    | a1, a2, a3, a4, a5, Flipped a when a.property.id = id ->
        remove_by_id id (a1, a2, a3, a4 ,a5, Nothing)
    | advs -> advs

  let inline set pos a =
    function
      a1, a2, a3, a4, a5, a6 ->
        match pos with
          First  -> a, a2, a3, a4, a5, a6
        | Second -> a1, a, a3, a4, a5, a6
        | Third  -> a1, a2, a, a4, a5, a6
        | Fourth -> a1, a2, a3, a, a5, a6
        | Fifth  -> a1, a2, a3, a4, a, a6
        | Sixth  -> a1, a2, a3, a4, a5, a


  let inline updated pos f advs =
    match pos, advs with
      First, (Exist a, a2, a3, a4, a5, a6) ->
        Exist (f a), a2, a3, a4, a5, a6
    | Second, (a1, Exist a, a3, a4, a5, a6) ->
        a1, Exist (f a), a3, a4, a5, a6
    | Third,  (a1, a2, Exist a, a4, a5, a6) ->
        a1, a2, Exist (f a), a4, a5, a6
    | Fourth, (a1, a2, a3, Exist a, a5, a6) ->
        a1, a2, a3, Exist (f a), a5, a6
    | Fifth, (a1, a2, a3, a4, Exist a, a6) ->
        a1, a2, a3, a4, Exist (f a), a6
    | Sixth, (a1, a2, a3, a4, a5, Exist a) ->
        a1, a2, a3, a4, a5, Exist (f a)
    | First, (Flipped a, a2, a3, a4, a5, a6) ->
        Flipped (f a), a2, a3, a4, a5, a6
    | Second, (a1, Flipped a, a3, a4, a5, a6) ->
        a1, Flipped (f a), a3, a4, a5, a6
    | Third, (a1, a2, Flipped a, a4, a5, a6) ->
        a1, a2, Flipped (f a), a4, a5, a6
    | Fourth, (a1, a2, a3, Flipped a, a5, a6) ->
        a1, a2, a3, Flipped (f a), a5, a6
    | Fifth, (a1, a2, a3, a4, Flipped a, a6) ->
        a1, a2, a3, a4, Flipped (f a), a6
    | Sixth, (a1, a2, a3, a4, a5, Flipped a) ->
        a1, a2, a3, a4, a5, Flipped (f a)
    | _, advs -> advs

  let foldl : ('a -> Cast.t -> 'a) -> 'a -> t -> 'a =
    fun f a ->
      to_list >> List.fold f a
  
  let fold = foldl

  let foldl_with_pos : ('a -> (Position * Cast.t) -> 'a) -> 'a -> t -> 'a =
    fun f a ->
      to_list >> List.zip positions >> List.fold f a

  let fold_with_pos = foldl_with_pos
      
  let forall : (Cast.t -> bool) -> t -> bool =
    fun f ->
      to_seq >> Seq.forall f

  let indexed : t -> (int * Cast.t) list =
    to_list >> List.indexed

  let try_find : (Cast.t -> bool) -> t -> Cast.t option =
    fun f ->
      to_seq >> Seq.tryFind f

  let try_find_with_position : (Cast.t -> bool) -> t -> (Position * Cast.t) option =
    fun f ->
      to_seq
      >> Seq.indexed
      >> Seq.map (function idx, cast -> int_to_pos idx, cast )
      >> Seq.tryFind (function _, cast -> f cast)

  let contains_by : (Cast.t -> bool) -> t -> bool =
    fun f -> 
      try_find f >> Option.isSome