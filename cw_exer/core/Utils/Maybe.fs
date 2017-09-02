namespace CardWirthEngine.Utils

module Maybe =
  type Maybe internal () =
    member this.Bind (m, f) = Option.bind m f
    member this.Return v = Some v

  let maybe = new Maybe ()