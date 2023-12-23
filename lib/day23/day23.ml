let get_start_and_end map =
  match
    ( Array.find_index (fun tile -> tile = '.') map.(0)
    , Array.find_index (fun tile -> tile = '.') map.(Array.length map - 1) )
  with
  | Some start_tile, Some end_tile ->
      ((start_tile, 0), (end_tile, Array.length map - 1))
  | _ ->
      failwith "Start or end position not found."

let find_longest_path tile_matcher map start_coords end_coords =
  let visited = Hashtbl.create (Array.length map * Array.length map.(0))
  and crossroads = Hashtbl.create 100
  and get_neighbors tile x_coord y_coord =
    let empty_tile_handler () =
      let orig_neighbors =
        Utils.get_neighbor_idxs_cardinal_with_directions (x_coord, y_coord)
          (Array.length map.(0))
          (Array.length map)
        |> List.filter (fun ((x_coord, y_coord), _) ->
               map.(y_coord).(x_coord) <> '#' )
      in
      let filtered_neighbors =
        orig_neighbors
        |> List.filter (fun ((x_coord, y_coord), from_direction) ->
               let neighbor = map.(y_coord).(x_coord) in
               not
                 ( (neighbor = '>' && from_direction = Utils.FromRight)
                 || (neighbor = '<' && from_direction = Utils.FromLeft)
                 || (neighbor = '^' && from_direction = Utils.FromTop)
                 || (neighbor = 'v' && from_direction = Utils.FromBottom) ) )
        |> List.map (fun (coords, _) -> coords)
      in
      (filtered_neighbors, List.length orig_neighbors)
    in
    tile_matcher empty_tile_handler tile x_coord y_coord
  in
  let rec take_hike queue =
    match queue with
    | [] ->
        ()
    | (((x_coord, y_coord) as coords), steps, last_crossroad) :: tail ->
        let tile = map.(y_coord).(x_coord) in
        let neighbors, all_neighbors_count =
          get_neighbors tile x_coord y_coord
        in
        let is_crossroads = all_neighbors_count > 2 || coords = end_coords in
        ( if is_crossroads then
            match Hashtbl.find_opt crossroads last_crossroad with
            | None ->
                Hashtbl.replace crossroads last_crossroad [(steps, coords)]
            | Some prev ->
                Hashtbl.replace crossroads last_crossroad
                  ((steps, coords) :: prev) ) ;
        let new_paths =
          if Hashtbl.mem visited coords then []
          else
            List.map
              (fun neighbor_coords ->
                ( neighbor_coords
                , (if is_crossroads then 1 else steps + 1)
                , if is_crossroads then coords else last_crossroad ) )
              neighbors
        in
        Hashtbl.replace visited coords true ;
        take_hike (new_paths @ tail)
  in
  take_hike [(start_coords, 0, start_coords)] ;
  let rec bfs queue max_steps =
    match queue with
    | [] ->
        max_steps
    | (coords, steps) :: tail ->
        if coords = end_coords then bfs tail (max steps max_steps)
        else
          let neighbors =
            Hashtbl.find crossroads coords
            |> List.map (fun (path_length, neighbor_coords) ->
                   (neighbor_coords, path_length + steps) )
          in
          bfs (neighbors @ tail) max_steps
  in
  bfs [(start_coords, 0)] 0

let solution_aux tile_matcher path =
  let map = Utils.file_to_grid path in
  let start_coords, end_coords = get_start_and_end map in
  find_longest_path tile_matcher map start_coords end_coords

let part_1_aux path =
  let tile_matcher empty_tile_handler tile x_coord y_coord =
    match tile with
    | '>' ->
        ([(x_coord + 1, y_coord)], 1)
    | '<' ->
        ([(x_coord - 1, y_coord)], 1)
    | 'v' ->
        ([(x_coord, y_coord + 1)], 1)
    | '^' ->
        ([(x_coord, y_coord - 1)], 1)
    | '.' ->
        empty_tile_handler ()
    | _ ->
        failwith ("Invalid tile: " ^ String.make 1 tile)
  in
  solution_aux tile_matcher path

let test_1 () =
  part_1_aux "lib/day23/test.txt" |> print_int ;
  print_newline ()

let part_1 () = part_1_aux "lib/day23/input.txt"

let part_2_aux path =
  let tile_matcher empty_tile_handler tile x_coord y_coord =
    match tile with
    | '<' | '>' | '^' | 'v' | '.' ->
        empty_tile_handler ()
    | _ ->
        failwith ("Invalid tile: " ^ String.make 1 tile)
  in
  solution_aux tile_matcher path

let test_2 () =
  part_2_aux "lib/day23/test.txt" |> print_int ;
  print_newline ()

let part_2 () = part_2_aux "lib/day23/input.txt"

let solution () =
  print_string "Part 1: " ;
  part_1 () |> print_int ;
  print_newline () ;
  print_string "Part 2: " ;
  part_2 () |> print_int ;
  print_newline ()
