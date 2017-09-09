namespace CardWirthEngine.Utils

module Maybe =
  type Maybe internal () =
    member this.Zero () = None
    member this.Bind (m, f) = Option.bind f m
    member this.Return v = Some v

  let c = new Maybe ()