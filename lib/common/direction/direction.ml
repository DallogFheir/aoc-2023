type direction = Left | Right | Top | Bottom

let get_opposite_direction dir =
  match dir with Left -> Right | Right -> Left | Top -> Bottom | Bottom -> Top
