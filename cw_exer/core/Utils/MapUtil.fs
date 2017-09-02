namespace CardWirthEngine.Utils

module MapUtil =
  let inline updated
    (key: 'K)
    (value: 'V)
    (map: ('K, 'V) Map)
    : ('K, 'V) Map
    = map.Add (key, value)

  let removed(key: 'K)(map: ('K, 'V) Map): ('K, 'V) Map =
    map.Remove key

