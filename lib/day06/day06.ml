let get_win_count time distance =
  let delta = (time * time) - (4 * distance) |> float_of_int in
  let x1 = (float_of_int time -. sqrt delta) /. 2.
  and x2Int = (float_of_int time +. sqrt delta) /. 2. |> ceil |> int_of_float in
  let x1Int = int_of_float (if Utils.is_int x1 then x1 +. 1. else ceil x1) in
  x2Int - x1Int

let part_1_aux path =
  let get_parameters part =
    Str.split (Str.regexp {| +|}) part
    |> List.tl
    |> List.map (fun value -> int_of_string value)
  in
  match Utils.file_to_list path with
  | [time_part; distance_part] ->
      let time_lst = get_parameters time_part
      and distance_lst = get_parameters distance_part in
      List.combine time_lst distance_lst
      |> List.fold_left
           (fun product (time, distance) ->
             product * get_win_count time distance )
           1
  | _ ->
      failwith "Invalid input."

let part_2_aux path =
  let get_parameter part =
    Str.split (Str.regexp {| +|}) part
    |> List.tl
    |> List.fold_left (fun acc num_str -> acc ^ num_str) ""
    |> int_of_string
  in
  match Utils.file_to_list path with
  | [time_part; distance_part] ->
      let time = get_parameter time_part
      and distance = get_parameter distance_part in
      get_win_count time distance
  | _ ->
      failwith "Invalid input."

let test_1 () =
  part_1_aux "lib/day06/test.txt" |> print_int ;
  print_newline ()

let part_1 () = part_1_aux "lib/day06/input.txt"

let test_2 () =
  part_2_aux "lib/day06/test.txt" |> print_int ;
  print_newline ()

let part_2 () = part_2_aux "lib/day06/input.txt"

let solution () =
  print_endline "DAY 6" ;
  print_string "Part 1: " ;
  part_1 () |> print_int ;
  print_newline () ;
  print_string "Part 2: " ;
  part_2 () |> print_int ;
  print_newline ()
