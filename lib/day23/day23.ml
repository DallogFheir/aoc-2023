let get_start_and_end map =
  match
    ( Array.find_index (fun tile -> tile = '.') map.(0)
    , Array.find_index (fun tile -> tile = '.') map.(Array.length map - 1) )
  with
  | Some start_tile, Some end_tile ->
      ((start_tile, 0), (end_tile, Array.length map - 1))
  | _ ->
      failwith "Start or end position not found."

let find_longest_path get_neighbors crossroads_preprocessor map start_coords
    end_coords =
  let visited = Hashtbl.create (Array.length map * Array.length map.(0))
  and crossroads = Hashtbl.create 100 in
  let rec take_hike queue =
    match queue with
    | [] ->
        ()
    | (((x_coord, y_coord) as coords), steps, last_crossroad) :: tail ->
        let tile = map.(y_coord).(x_coord) in
        let neighbors, all_neighbors_count =
          get_neighbors tile map x_coord y_coord
        in
        let is_crossroads = all_neighbors_count > 2 || coords = end_coords in
        ( if is_crossroads && coords <> last_crossroad then
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
    | (coords, steps, visited_on_path) :: tail ->
        if Hashtbl.mem visited_on_path coords then bfs tail max_steps
        else if coords = end_coords then bfs tail (max steps max_steps)
        else (
          Hashtbl.replace visited_on_path coords true ;
          let neighbors =
            Hashtbl.find crossroads coords
            |> List.map (fun (path_length, neighbor_coords) ->
                   ( neighbor_coords
                   , path_length + steps
                   , Hashtbl.copy visited_on_path ) )
          in
          bfs (neighbors @ tail) max_steps )
  in
  crossroads_preprocessor crossroads ;
  bfs [(start_coords, 0, Hashtbl.create 100)] 0

let solution_aux get_neighbors crossroads_preprocessor path =
  let map = Utils.file_to_grid path in
  let start_coords, end_coords = get_start_and_end map in
  find_longest_path get_neighbors crossroads_preprocessor map start_coords
    end_coords

let part_1_aux path =
  let get_neighbors tile map x_coord y_coord =
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
                   ( (neighbor = '>' && from_direction = Direction.Right)
                   || (neighbor = '<' && from_direction = Direction.Left)
                   || (neighbor = '^' && from_direction = Direction.Top)
                   || (neighbor = 'v' && from_direction = Direction.Bottom) ) )
          |> List.map (fun (coords, _) -> coords)
        in
        (filtered_neighbors, List.length orig_neighbors)
    | _ ->
        failwith ("Invalid tile: " ^ String.make 1 tile)
  in
  solution_aux get_neighbors (fun _ -> ()) path

let test_1 () =
  part_1_aux "lib/day23/test.txt" |> print_int ;
  print_newline ()

let part_1 () = part_1_aux "lib/day23/input.txt"

let part_2_aux path =
  let get_neighbors tile map x_coord y_coord =
    match tile with
    | '>' | '<' | 'v' | '^' | '.' ->
        let orig_neighbors =
          Utils.get_neighbor_idxs_cardinal (x_coord, y_coord)
            (Array.length map.(0))
            (Array.length map)
          |> List.filter (fun (x_coord, y_coord) ->
                 map.(y_coord).(x_coord) <> '#' )
        in
        (orig_neighbors, List.length orig_neighbors)
    | _ ->
        failwith ("Invalid tile: " ^ String.make 1 tile)
  and crossroads_preprocessor crossroads =
    Hashtbl.iter
      (fun coords neighbor_lst ->
        List.iter
          (fun (steps, neighbor_coords) ->
            let new_neighbor_lst =
              match Hashtbl.find_opt crossroads neighbor_coords with
              | None ->
                  [(steps, coords)]
              | Some prev ->
                  let new_neighbor = (steps, coords) in
                  if List.mem new_neighbor prev then prev
                  else new_neighbor :: prev
            in
            Hashtbl.replace crossroads neighbor_coords new_neighbor_lst )
          neighbor_lst )
      crossroads
  in
  solution_aux get_neighbors crossroads_preprocessor path

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
