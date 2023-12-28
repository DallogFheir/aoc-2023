type direction = Left | Right | Up | Down

let get_opposite_direction dir =
  match dir with Left -> Right | Right -> Left | Up -> Down | Down -> Up
