namespace CardWirthEngine.GameMasters.Cards

open CardWirthEngine.Utils
open CardWirthEngine.Data.Type
open CardWirthEngine.Cards

module Adventurers =

  let party_limit = 6

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
    with
      member this.cast =
        match this with
          Exist cast -> cast
        | Flipped cast -> cast

  type t = CardState array

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

  let length : t -> int = Array.length
  let to_list : t -> CardState list = Array.toList
  let to_list_with_pos : t -> (Position * CardState) list =
    to_list
    >> List.indexed
    >> List.map (function idx, cast -> int_to_pos idx, cast)
  let to_seq : t -> CardState seq = Array.toSeq
  let to_seq_with_pos : t -> (Position * CardState) seq =
    to_seq
    >> Seq.indexed
    >> Seq.map (function idx, cast -> int_to_pos idx, cast)

  let to_cast_list : bool -> t -> Cast.t list =
    fun include_flipped ->
      Array.fold
        (fun list ->
          function
            Exist c -> c :: list
          | Flipped c when include_flipped -> c :: list
          | _ -> list)
        []

  let to_cast_seq : bool -> t -> Cast.t seq =
    fun include_flipped ->
      to_cast_list include_flipped >> List.toSeq

  let get_by_index : int -> t -> CardState =
    fun idx ->
      Array.tryItem idx
      >> function
           Some cast -> cast
         | _ -> raise <| InvalidAdventurerIndexException idx

  let get : Position -> t -> CardState =
    pos_to_int >> get_by_index

  let add : Cast.t -> t -> t =
    fun cast advs ->
      if length advs >= party_limit
      then advs
      else Array.append advs [|Exist cast|]

  let remove_by_index : int -> t -> t = Array.remove

  let remove : Position -> t -> t =
    pos_to_int >> remove_by_index

  let remove_by_id : CastId -> t -> t =
    fun id advs ->
      advs
      |> Array.tryFindIndex begin
           function
             Exist cast when cast.property.id = id -> true
           | Flipped cast when cast.property.id = id -> true
           | _ -> false end 
      |> Option.fold
           (fun _ idx -> remove_by_index idx advs)
           advs

  let set_by_index : int -> Cast.t -> t -> t =
    fun idx cast ->
      Array.updated idx <| Exist cast

  let set : Position -> Cast.t -> t -> t =
    pos_to_int >> set_by_index

  let update_by_index : int -> (Cast.t -> Cast.t) -> t -> t =
    fun idx f advs ->
      let adv =
        match get_by_index idx advs with
          Exist cast -> f cast
        | Flipped cast -> f cast
      set_by_index idx adv advs

  let update : Position -> (Cast.t -> Cast.t) -> t -> t =
    pos_to_int >> update_by_index

  let inline fc'' flipped f a =
    function
      Exist cast -> f a cast
    | Flipped cast when flipped -> f a cast
    | _ -> a

  let inline fc f a = fc'' true f a
  let inline fc' f a = fc'' false f a

  let foldl : ('a -> CardState -> 'a) -> 'a -> t -> 'a = Array.fold
  
  let fold = foldl

  let foldl_with_pos : ('a -> (Position * CardState) -> 'a) -> 'a -> t -> 'a =
    fun f a ->
      to_list >> List.zip positions >> List.fold f a

  let fold_with_pos = foldl_with_pos
      
  let forall : (CardState -> bool) -> t -> bool = Array.forall

  let indexed : t -> (int * CardState) array = Array.indexed

  let try_find : (CardState -> bool) -> t -> CardState option = Array.tryFind

  let try_find_with_position : (CardState -> bool) -> t -> (Position * CardState) option =
    fun f ->
      Array.indexed
      >> Array.tryFind (function idx, cast -> f cast)
      >> Option.map (function idx, cast -> int_to_pos idx, cast)

  let exists : (CardState -> bool) -> t -> bool = Array.exists

  let inline cc'' flipped f =
    function
      Exist cast -> Exist (f cast)
    | Flipped cast when flipped -> Flipped (f cast)
    | Flipped _ as flipped -> flipped

  let inline cc f = cc'' true f
  let inline cc' f = cc'' false f

  let map : (CardState -> CardState) -> t -> t = Array.map