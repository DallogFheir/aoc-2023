let parse_input lines =
  List.map
    (fun line ->
      match String.split_on_char ' ' line with
      | [direction_str; length_str; color_str] ->
          let direction =
            match direction_str with
            | "R" ->
                Direction.Right
            | "L" ->
                Direction.Left
            | "U" ->
                Direction.Up
            | "D" ->
                Direction.Down
            | _ ->
                failwith ("Invalid direction: " ^ direction_str)
          and length = int_of_string length_str
          and color = String.sub color_str 2 (String.length color_str - 3) in
          (direction, length, color)
      | _ ->
          failwith ("Invalid input: " ^ line) )
    lines

let move_in_direction (x_coord, y_coord) direction length =
  match direction with
  | Direction.Left ->
      (x_coord - length, y_coord)
  | Direction.Right ->
      (x_coord + length, y_coord)
  | Direction.Up ->
      (x_coord, y_coord - length)
  | Direction.Down ->
      (x_coord, y_coord + length)

let get_points instructions =
  let _, points, count =
    List.fold_left
      (fun (current_point, acc, count) (direction, length, _) ->
        let next_point = move_in_direction current_point direction length in
        (next_point, next_point :: acc, count + length) )
      ((0, 0), [], 0)
      instructions
  in
  (points, count)

let calculate_shoelace points =
  let last_opt, sum =
    List.fold_left
      (fun (prev_opt, sum) ((x_coord, y_coord) as point) ->
        let to_add =
          match prev_opt with
          | Some (x_coord_prev, y_coord_prev) ->
              (y_coord * x_coord_prev) - (x_coord * y_coord_prev)
          | None ->
              0
        in
        (Some point, sum + to_add) )
      (None, 0) points
  in
  match last_opt with
  | Some (x_coord_last, y_coord_last) ->
      let x_coord_first, y_coord_first = List.hd points in
      let final_sum =
        sum + (x_coord_last * y_coord_first) - (y_coord_last * x_coord_first)
      in
      -int_of_float (0.5 *. float_of_int final_sum)
  | None ->
      failwith "List is empty."

let calculate_interior_count_from_pick boundary_count area =
  area - (boundary_count / 2) + 1

let solution_aux path parse_input_fn =
  let points, boundary_count =
    Utils.file_to_list path |> parse_input_fn |> get_points
  in
  ( points |> calculate_shoelace
  |> calculate_interior_count_from_pick boundary_count )
  + boundary_count

let part_1_aux path = solution_aux path parse_input

let test_1 () =
  part_1_aux "lib/day18/test.txt" |> print_int ;
  print_newline ()

let part_1 () = part_1_aux "lib/day18/input.txt"

let part_2_aux path =
  solution_aux path (fun input ->
      parse_input input
      |> List.map (fun (_, _, color) ->
             let color_last_idx = String.length color - 1 in
             let encoded_distance = String.sub color 0 color_last_idx
             and direction_code = color.[color_last_idx] in
             let distance = int_of_string ("0x" ^ encoded_distance)
             and direction =
               [|Direction.Right; Direction.Down; Direction.Left; Direction.Up|].(
               Utils.digit_char_to_number direction_code)
             in
             (direction, distance, color) ) )

let test_2 () =
  part_2_aux "lib/day18/test.txt" |> print_int ;
  print_newline ()

let part_2 () = part_2_aux "lib/day18/input.txt"

let solution () =
  print_string "Part 1: " ;
  part_1 () |> print_int ;
  print_newline () ;
  print_string "Part 2: " ;
  part_2 () |> print_int ;
  print_newline ()
