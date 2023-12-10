type cube = Red of int | Green of int | Blue of int

let find_each_grab game =
  let game_pattern = Str.regexp {|^Game \([0-9]+\): \(.+\)|} in
  let m = Str.string_match game_pattern game 0 in
  if m then
    let game_id = Str.matched_group 1 game |> int_of_string
    and rest = Str.matched_group 2 game in
    ( game_id
    , Str.split (Str.regexp "; ") rest
      |> List.map (fun grab ->
             Str.split (Str.regexp ", ") grab
             |> List.map (fun cube ->
                    match String.split_on_char ' ' cube with
                    | num_str :: tail -> (
                        let color = List.hd tail
                        and num = int_of_string num_str in
                        match color with
                        | "red" ->
                            Red num
                        | "green" ->
                            Green num
                        | "blue" ->
                            Blue num
                        | _ ->
                            failwith ("Invalid color: " ^ color) )
                    | _ ->
                        failwith ("Cannot split " ^ cube ^ " on space.") ) ) )
  else failwith (game ^ " does not match game pattern.")

let check_if_possible game =
  let max_reds = 12 and max_greens = 13 and max_blues = 14 in
  List.for_all
    (fun grab ->
      List.for_all
        (fun cube ->
          match cube with
          | Red num ->
              num <= max_reds
          | Green num ->
              num <= max_greens
          | Blue num ->
              num <= max_blues )
        grab )
    game

let find_minimum game =
  let rec find_minimum_in_grab grab acc =
    let current_min_red, current_min_green, current_min_blue = acc in
    match grab with
    | Red num :: tail ->
        find_minimum_in_grab tail
          (max num current_min_red, current_min_green, current_min_blue)
    | Green num :: tail ->
        find_minimum_in_grab tail
          (current_min_red, max num current_min_green, current_min_blue)
    | Blue num :: tail ->
        find_minimum_in_grab tail
          (current_min_red, current_min_green, max num current_min_blue)
    | _ ->
        acc
  in
  List.fold_left
    (fun acc grab ->
      let current_min_red, current_min_green, current_min_blue = acc
      and new_min_red, new_min_green, new_min_blue =
        find_minimum_in_grab grab (0, 0, 0)
      in
      ( max current_min_red new_min_red
      , max current_min_green new_min_green
      , max current_min_blue new_min_blue ) )
    (0, 0, 0) game

let get_power_set (red, green, blue) = red * green * blue

let part_1_aux path =
  Utils.file_to_list path
  |> List.fold_left
       (fun acc game ->
         let game_id, grabs = find_each_grab game in
         (if check_if_possible grabs then game_id else 0) + acc )
       0

let test_1 () =
  part_1_aux "lib/day02/test.txt" |> print_int ;
  print_newline ()

let part_1 () = part_1_aux "lib/day02/input.txt"

let part_2_aux path =
  Utils.file_to_list path
  |> List.fold_left
       (fun acc game ->
         let _, grabs = find_each_grab game in
         (find_minimum grabs |> get_power_set) + acc )
       0

let test_2 () =
  part_2_aux "lib/day02/test.txt" |> print_int ;
  print_newline ()

let part_2 () = part_2_aux "lib/day02/input.txt"

let solution () =
  print_string "Part 1: " ;
  part_1 () |> print_int ;
  print_newline () ;
  print_string "Part 2: " ;
  part_2 () |> print_int ;
  print_newline ()
