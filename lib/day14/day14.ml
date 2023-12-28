type direction = North | East | South | West

let tilt direction grid =
  let new_grid, line_reverser, grid_transposer =
    match direction with
    | North ->
        (Utils.transpose grid, (fun line -> line), Utils.transpose)
    | East ->
        (Utils.clone_2d_grid grid, Utils.reverse_array, fun grid -> grid)
    | South ->
        (Utils.transpose grid, Utils.reverse_array, Utils.transpose)
    | West ->
        (Utils.clone_2d_grid grid, (fun line -> line), fun grid -> grid)
  in
  Array.iteri
    (fun idx line ->
      let new_line = line_reverser line in
      let parts, (last_idx, last_round_rock_count, last_part_start) =
        Array.fold_left
          (fun (lst, (idx, round_rock_count, part_start)) symbol ->
            match symbol with
            | '#' ->
                ( ( if round_rock_count > 0 then
                      (round_rock_count, part_start, idx - 1) :: lst
                    else lst )
                , (idx + 1, 0, idx + 1) )
            | 'O' ->
                (lst, (idx + 1, round_rock_count + 1, part_start))
            | '.' ->
                (lst, (idx + 1, round_rock_count, part_start))
            | _ ->
                failwith ("Invalid symbol: " ^ String.make 1 symbol) )
          ([], (0, 0, 0))
          new_line
      in
      let final_parts =
        if last_round_rock_count > 0 then
          (last_round_rock_count, last_part_start, last_idx - 1) :: parts
        else parts
      in
      List.iter
        (fun (round_rock_count, part_start, part_end) ->
          Utils.loop
            (fun idx ->
              let relative_idx = idx - part_start in
              new_line.(idx) <-
                (if relative_idx < round_rock_count then 'O' else '.') )
            part_start part_end )
        final_parts ;
      new_grid.(idx) <- line_reverser new_line )
    new_grid ;
  grid_transposer new_grid

let calculate_load rock_grid =
  Array.fold_left
    (fun (idx, sum) line ->
      let round_rock_count =
        Utils.array_count_if (fun symbol -> symbol = 'O') line
      in
      (idx - 1, sum + (round_rock_count * idx)) )
    (Array.length rock_grid, 0)
    rock_grid
  |> snd

let part_1_aux path = Utils.file_to_grid path |> tilt North |> calculate_load

let test_1 () =
  part_1_aux "lib/day14/test.txt" |> print_int ;
  print_newline ()

let part_1 () = part_1_aux "lib/day14/input.txt"

let part_2_aux path =
  let cache = Hashtbl.create 1000 and target = 1000000000 in
  let rec cycle current_idx grid =
    if current_idx > target then grid
    else
      match Hashtbl.find_opt cache grid with
      | Some found_idx -> (
          let cycle_size = current_idx - found_idx
          and adjusted_target = target - (found_idx - 1) in
          let res_idx = (adjusted_target mod cycle_size) - 1 + found_idx in
          match
            Hashtbl.to_seq cache |> Seq.find (fun (_, idx) -> idx = res_idx)
          with
          | Some (grid, _) ->
              grid
          | None ->
              failwith ("Did not find grid with index " ^ string_of_int res_idx)
          )
      | None ->
          Hashtbl.add cache grid current_idx ;
          cycle (current_idx + 1)
            (grid |> tilt North |> tilt West |> tilt South |> tilt East)
  in
  Utils.file_to_grid path |> cycle 0 |> calculate_load

let test_2 () =
  part_2_aux "lib/day14/test.txt" |> print_int ;
  print_newline ()

let part_2 () = part_2_aux "lib/day14/input.txt"

let solution () =
  print_endline "DAY 14" ;
  print_string "Part 1: " ;
  part_1 () |> print_int ;
  print_newline () ;
  print_string "Part 2: " ;
  part_2 () |> print_int ;
  print_newline ()
