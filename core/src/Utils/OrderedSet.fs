namespace CardWirthEngine.Utils

module OrderedSet =
  type 'a t when 'a : comparison =
    { set : 'a Set
    ; list : 'a list
    }

  let of_list : 'a list -> 'a t =
    fun list ->
      let list' = List.distinct list in
      let set = Set.ofList list' in
      { set = set; list = list' }
  
  let of_seq : 'a seq -> 'a t =
    fun seq ->
      seq
      |> Seq.toList 
      |> of_list

  let add : 'a -> 'a t -> 'a t =
    fun e ->
      function
        { set = set } as oset when Set.contains e set ->
          oset
      | { set = set; list = list } ->
          { set = Set.add e set
          ; list = e :: list
          }

  let remove : 'a -> 'a t -> 'a t =
    fun e ->
      function
        { set = set; list = list } as oset when Set.contains e set ->
          { set = Set.remove e set
          ; list = List.filter ((<>) e) list
          }
      | oset ->
          oset