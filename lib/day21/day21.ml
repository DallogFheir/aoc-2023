let bfs start_pos map =
  let get_neighbors x_coord y_coord =
    [ (x_coord + 1, y_coord)
    ; (x_coord - 1, y_coord)
    ; (x_coord, y_coord + 1)
    ; (x_coord, y_coord - 1) ]
  and col_length = Array.length map
  and row_length = Array.length map.(0) in
  let visited = Hashtbl.create (row_length * col_length) in
  let rec bfs_aux queue =
    match queue with
    | (((x, y) as coords), steps) :: tail ->
        if Hashtbl.mem visited coords then bfs_aux tail
        else (
          Hashtbl.replace visited coords steps ;
          let neighbors = get_neighbors x y in
          let reachable_neighbors =
            List.filter
              (fun (neighbor_x, neighbor_y) ->
                neighbor_x >= 0 && neighbor_y >= 0 && neighbor_x < col_length
                && neighbor_y < row_length
                && map.(neighbor_x).(neighbor_y) <> '#' )
              neighbors
            |> List.map (fun neighbor -> (neighbor, steps + 1))
          in
          bfs_aux (tail @ reachable_neighbors) )
    | _ ->
        visited
  in
  bfs_aux [(start_pos, 0)]

let find_start_position map =
  let rec find_start_position_aux x_coord y_coord =
    if x_coord >= Array.length map then
      failwith "Did not find starting position in map."
    else if y_coord >= Array.length map.(0) then
      find_start_position_aux (x_coord + 1) 0
    else if map.(x_coord).(y_coord) = 'S' then (x_coord, y_coord)
    else find_start_position_aux x_coord (y_coord + 1)
  in
  find_start_position_aux 0 0

let count_steps condition visited =
  Hashtbl.to_seq_values visited
  |> Seq.filter (fun step -> condition step)
  |> Seq.length

let part_1_aux path steps =
  let map = Utils.file_to_grid path in
  let start_pos = find_start_position map in
  let visited = bfs start_pos map in
  count_steps (fun step -> step mod 2 = 0 && step <= steps) visited

let test_1 () =
  part_1_aux "lib/day21/test.txt" 6 |> print_int ;
  print_newline ()

let part_1 () = part_1_aux "lib/day21/input.txt" 64

let part_2_aux path steps =
  let map = Utils.file_to_grid path in
  let start_pos = find_start_position map in
  let visited = bfs start_pos map and dist_to_edge = Array.length map / 2 in
  let boundary_crosses = (steps - dist_to_edge) / Array.length map in
  let same_as_start_tiles_count =
    int_of_float (float_of_int (boundary_crosses + 1) ** 2.)
  and same_as_start_steps =
    count_steps (fun step -> step mod 2 = steps mod 2) visited
  and other_than_start_tiles_count =
    int_of_float (float_of_int boundary_crosses ** 2.)
  and other_than_start_steps =
    count_steps (fun step -> step mod 2 <> steps mod 2) visited
  and same_as_start_corners =
    count_steps
      (fun step -> step > dist_to_edge && step mod 2 = steps mod 2)
      visited
  and other_than_start_corners =
    count_steps
      (fun step -> step > dist_to_edge && step mod 2 <> steps mod 2)
      visited
    - 1
  in
  (same_as_start_tiles_count * same_as_start_steps)
  + (other_than_start_tiles_count * other_than_start_steps)
  - (same_as_start_corners * (boundary_crosses + 1))
  + (other_than_start_corners * boundary_crosses)

let part_2 () = part_2_aux "lib/day21/input.txt" 26501365

let solution () =
  print_string "Part 1: " ;
  part_1 () |> print_int ;
  print_newline () ;
  print_string "Part 2: " ;
  part_2 () |> print_int ;
  print_newline ()
