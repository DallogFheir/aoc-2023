type direction = North | East | South | West

let find_start_neighbors grid (start_row_idx, start_col_idx) =
  let to_north = ['|'; '7'; 'F']
  and to_east = ['-'; 'J'; '7']
  and to_south = ['|'; 'L'; 'J']
  and to_west = ['-'; 'L'; 'F'] in
  let north =
    ( South
    , (start_row_idx - 1, start_col_idx)
    , start_row_idx - 1 >= 0
      && List.mem grid.(start_row_idx - 1).(start_col_idx) to_north )
  and east =
    ( West
    , (start_row_idx, start_col_idx + 1)
    , start_col_idx + 1 < Array.length grid.(0)
      && List.mem grid.(start_row_idx).(start_col_idx + 1) to_east )
  and south =
    ( North
    , (start_row_idx + 1, start_col_idx)
    , start_row_idx + 1 < Array.length grid
      && List.mem grid.(start_row_idx + 1).(start_col_idx) to_south )
  and west =
    ( East
    , (start_row_idx, start_col_idx - 1)
    , start_col_idx - 1 >= 0
      && List.mem grid.(start_row_idx).(start_col_idx - 1) to_west )
  in
  List.filter_map
    (fun (dir, coords, is_neighbor) ->
      if is_neighbor then Some (coords, dir) else None )
    [north; east; south; west]

let get_next_coords_and_dir (row_idx, col_idx) dir pipe =
  let next_dir =
    match (pipe, dir) with
    | '|', (North as dir)
    | '|', (South as dir)
    | '-', (East as dir)
    | '-', (West as dir) ->
        dir
    | 'L', North ->
        West
    | 'L', East ->
        South
    | 'J', North ->
        East
    | 'J', West ->
        South
    | '7', South ->
        East
    | '7', West ->
        North
    | 'F', South ->
        West
    | 'F', East ->
        North
    | _ ->
        failwith ("Invalid pipe: " ^ String.make 1 pipe)
  in
  match next_dir with
  | North ->
      ((row_idx + 1, col_idx), North)
  | East ->
      ((row_idx, col_idx - 1), East)
  | South ->
      ((row_idx - 1, col_idx), South)
  | West ->
      ((row_idx, col_idx + 1), West)

let find_start grid =
  let rec find_start_aux row_idx col_idx =
    if row_idx >= Array.length grid then failwith "Start not found."
    else if col_idx >= Array.length grid.(0) then find_start_aux (row_idx + 1) 0
    else if grid.(row_idx).(col_idx) = 'S' then (row_idx, col_idx)
    else find_start_aux row_idx (col_idx + 1)
  in
  find_start_aux 0 0

let part_1_aux path =
  let grid = Utils.file_to_grid path in
  let start_coords = find_start grid in
  let start_neighbors = find_start_neighbors grid start_coords in
  let rec get_loop_length (((row_idx_1, col_idx_1) as coords_1), dir_1)
      (((row_idx_2, col_idx_2) as coords_2), dir_2) count =
    if coords_1 = coords_2 then count + 1
    else
      get_loop_length
        (get_next_coords_and_dir coords_1 dir_1 grid.(row_idx_1).(col_idx_1))
        (get_next_coords_and_dir coords_2 dir_2 grid.(row_idx_2).(col_idx_2))
        (count + 1)
  in
  match start_neighbors with
  | [neighbor_1; neighbor_2] ->
      get_loop_length neighbor_1 neighbor_2 0
  | _ ->
      failwith "Start should have exactly 2 neighbors."

let get_neighbor_idxs_cardinal_with_extra_border (row_idx, col_idx) row_length
    col_length =
  let addends = List.init 3 (fun idx -> idx - 1)
  and is_valid_idx idx bound = idx >= -1 && idx <= bound in
  addends
  |> List.map (fun row_addend ->
         addends
         |> List.filter_map (fun col_addend ->
                let row_coord = row_idx + row_addend
                and col_coord = col_idx + col_addend in
                if
                  (row_addend = 0 || col_addend = 0)
                  && row_addend <> col_addend
                  && is_valid_idx row_coord row_length
                  && is_valid_idx col_coord col_length
                then Some (row_coord, col_coord)
                else None ) )
  |> List.flatten

let get_outside_tiles pipes row_length col_length =
  let visited = Hashtbl.create (row_length * col_length) in
  let is_in_grid (row_idx, col_idx) =
    row_idx >= 0 && row_idx < row_length && col_idx >= 0 && col_idx < col_length
  in
  let rec get_outside_tiles_aux queue outside_acc =
    match queue with
    | head :: tail ->
        if Hashtbl.mem visited head then get_outside_tiles_aux tail outside_acc
        else (
          Hashtbl.add visited head true ;
          if Hashtbl.mem pipes head then get_outside_tiles_aux tail outside_acc
          else
            let neighbors =
              get_neighbor_idxs_cardinal_with_extra_border head row_length
                col_length
            in
            get_outside_tiles_aux (neighbors @ tail)
              (if is_in_grid head then head :: outside_acc else outside_acc) )
    | _ ->
        outside_acc
  in
  get_outside_tiles_aux [(-1, -1)] []

let part_2_aux path =
  let grid = Utils.file_to_grid path in
  let start_coords = find_start grid in
  let start_neighbors = find_start_neighbors grid start_coords in
  let rec get_all_coords_doubled_and_count (((row_idx, col_idx) as coords), dir)
      count coords_set =
    Hashtbl.add coords_set (2 * row_idx, 2 * col_idx) true ;
    ( match dir with
    | North ->
        Hashtbl.add coords_set ((2 * row_idx) - 1, 2 * col_idx) true
    | East ->
        Hashtbl.add coords_set (2 * row_idx, (2 * col_idx) + 1) true
    | South ->
        Hashtbl.add coords_set ((2 * row_idx) + 1, 2 * col_idx) true
    | West ->
        Hashtbl.add coords_set (2 * row_idx, (2 * col_idx) - 1) true ) ;
    if coords = start_coords then (coords_set, count + 1)
    else
      get_all_coords_doubled_and_count
        (get_next_coords_and_dir coords dir grid.(row_idx).(col_idx))
        (count + 1) coords_set
  in
  match start_neighbors with
  | [neighbor; _] ->
      let row_length = Array.length grid
      and col_length = Array.length grid.(0) in
      let pipes, pipe_count =
        Hashtbl.create (4 * row_length * col_length)
        |> get_all_coords_doubled_and_count neighbor 0
      in
      let outside_count =
        get_outside_tiles pipes (2 * row_length) (2 * col_length)
        |> List.filter (fun (row_idx, col_idx) ->
               row_idx mod 2 = 0 && col_idx mod 2 = 0 )
        |> List.length
      in
      (row_length * col_length) - pipe_count - outside_count
  | _ ->
      failwith "Start should have exactly 2 neighbors."

let test_1 () =
  part_1_aux "lib/day10/test1.txt" |> print_int ;
  print_newline () ;
  part_1_aux "lib/day10/test2.txt" |> print_int ;
  print_newline ()

let part_1 () = part_1_aux "lib/day10/input.txt"

let test_2 () =
  part_2_aux "lib/day10/test3.txt" |> print_int ;
  print_newline () ;
  part_2_aux "lib/day10/test4.txt" |> print_int ;
  print_newline () ;
  part_2_aux "lib/day10/test5.txt" |> print_int ;
  print_newline ()

let part_2 () = part_2_aux "lib/day10/input.txt"

let solution () =
  print_endline "DAY 10" ;
  print_string "Part 1: " ;
  part_1 () |> print_int ;
  print_newline () ;
  print_string "Part 2: " ;
  part_2 () |> print_int ;
  print_newline ()
