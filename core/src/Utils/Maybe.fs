namespace CardWirthEngine.Utils

module Maybe =
  type Maybe internal () =
    member __.Zero () = None
    member __.Bind (m, f) = Option.bind f m
    member __.Return v = Some v

  let c = new Maybe ()