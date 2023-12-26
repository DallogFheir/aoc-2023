type direction = FromLeft | FromRight | FromTop | FromBottom

let get_opposite_direction dir =
  match dir with
  | FromLeft ->
      FromRight
  | FromRight ->
      FromLeft
  | FromTop ->
      FromBottom
  | FromBottom ->
      FromTop
