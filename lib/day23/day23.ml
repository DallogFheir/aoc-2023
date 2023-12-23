let get_start_and_end map =
  match
    ( Array.find_index (fun tile -> tile = '.') map.(0)
    , Array.find_index (fun tile -> tile = '.') map.(Array.length map - 1) )
  with
  | Some start_tile, Some end_tile ->
      ((start_tile, 0), (end_tile, Array.length map - 1))
  | _ ->
      failwith "Start or end position not found."

let find_longest_path map start_pos end_pos =
  let rec find_longest_path_aux queue max_steps =
    match queue with
    | [] ->
        max_steps
    | (((x_coord, y_coord) as coords), steps, visited) :: tail ->
        if Hashtbl.mem visited coords then find_longest_path_aux tail max_steps
        else (
          Hashtbl.replace visited coords true ;
          if coords = end_pos then
            find_longest_path_aux tail (max steps max_steps)
          else
            let tile = map.(y_coord).(x_coord) in
            let to_add_to_queue =
              match tile with
              | '>' ->
                  [(x_coord + 1, y_coord)]
              | '<' ->
                  [(x_coord - 1, y_coord)]
              | 'v' ->
                  [(x_coord, y_coord + 1)]
              | '^' ->
                  [(x_coord, y_coord - 1)]
              | '.' ->
                  Utils.get_neighbor_idxs_cardinal coords
                    (Array.length map.(0))
                    (Array.length map)
                  |> List.filter_map (fun ((x_coord, y_coord) as coords) ->
                         if map.(y_coord).(x_coord) = '#' then None
                         else Some coords )
              | _ ->
                  failwith ("Invalid tile: " ^ String.make 1 tile)
            in
            if List.length to_add_to_queue = 1 then
              find_longest_path_aux
                ((List.hd to_add_to_queue, steps + 1, visited) :: tail)
                max_steps
            else
              let new_paths =
                List.map
                  (fun pos -> (pos, steps + 1, Hashtbl.copy visited))
                  to_add_to_queue
              in
              find_longest_path_aux (new_paths @ tail) max_steps )
  in
  find_longest_path_aux
    [(start_pos, 0, Hashtbl.create (Array.length map * Array.length map.(0)))]
    0

let part_1_aux path =
  let map = Utils.file_to_grid path in
  let start_pos, end_pos = get_start_and_end map in
  find_longest_path map start_pos end_pos

let test_1 () =
  part_1_aux "lib/day23/test.txt" |> print_int ;
  print_newline ()

let part_1 () = part_1_aux "lib/day23/input.txt"
(*
   let test_2 () =
     part_2_aux "lib/day23/test.txt" |> print_int ;
     print_newline ()

   let part_2 () = part_2_aux "lib/day23/input.txt"*)

let solution () =
  print_string "Part 1: " ;
  part_1 () |> print_int ;
  print_newline ()
(* ;
   print_string "Part 2: " ;
   part_2 () |> print_int ;
   print_newline () *)
