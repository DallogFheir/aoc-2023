type direction = FromLeft | FromRight | FromUp | FromDown

let get_opposite_direction direction =
  match direction with
  | FromLeft ->
      FromRight
  | FromRight ->
      FromLeft
  | FromUp ->
      FromDown
  | FromDown ->
      FromUp

let get_next_coord x_coord y_coord from_direction =
  match from_direction with
  | FromLeft ->
      (x_coord + 1, y_coord)
  | FromRight ->
      (x_coord - 1, y_coord)
  | FromUp ->
      (x_coord, y_coord + 1)
  | FromDown ->
      (x_coord, y_coord - 1)

let get_coords_after_mirror x_coord y_coord from_direction mirror =
  match (mirror, from_direction) with
  | '\\', FromLeft ->
      ((x_coord, y_coord + 1), FromUp)
  | '\\', FromRight ->
      ((x_coord, y_coord - 1), FromDown)
  | '\\', FromUp ->
      ((x_coord + 1, y_coord), FromLeft)
  | '\\', FromDown ->
      ((x_coord - 1, y_coord), FromRight)
  | '/', FromLeft ->
      ((x_coord, y_coord - 1), FromDown)
  | '/', FromRight ->
      ((x_coord, y_coord + 1), FromUp)
  | '/', FromUp ->
      ((x_coord - 1, y_coord), FromRight)
  | '/', FromDown ->
      ((x_coord + 1, y_coord), FromLeft)
  | _ ->
      failwith ("Invalid mirror: " ^ String.make 1 mirror)

let get_coords_after_splitter x_coord y_coord from_direction splitter =
  match (splitter, from_direction) with
  | '-', FromLeft | '-', FromRight | '|', FromDown | '|', FromUp ->
      [(get_next_coord x_coord y_coord from_direction, from_direction)]
  | '-', FromUp | '-', FromDown ->
      [((x_coord - 1, y_coord), FromRight); ((x_coord + 1, y_coord), FromLeft)]
  | '|', FromLeft | '|', FromRight ->
      [((x_coord, y_coord - 1), FromDown); ((x_coord, y_coord + 1), FromUp)]
  | _ ->
      failwith ("Invalid splitter: " ^ String.make 1 splitter)

let get_energized_tiles start_point start_direction grid =
  let light_beams = Hashtbl.create (Array.length grid * Array.length grid.(0))
  and light_beam_queue = [(start_point, start_direction)] in
  let rec add_to_queue_and_next x_coord y_coord from_direction tile tail =
    match tile with
    | '.' ->
        let next_x_coord, next_y_coord =
          get_next_coord x_coord y_coord from_direction
        in
        simulate_light_beams
          (((next_x_coord, next_y_coord), from_direction) :: tail)
    | '\\' | '/' ->
        let next_beam =
          get_coords_after_mirror x_coord y_coord from_direction tile
        in
        simulate_light_beams (next_beam :: tail)
    | '-' | '|' ->
        let next_beams =
          get_coords_after_splitter x_coord y_coord from_direction tile
        in
        simulate_light_beams (next_beams @ tail)
    | _ ->
        failwith ("Invalid symbol: " ^ String.make 1 tile)
  and simulate_light_beams queue =
    match queue with
    | ((x_coord, y_coord), from_direction) :: tail ->
        if Utils.are_coords_valid x_coord y_coord grid then
          let tile = grid.(y_coord).(x_coord) in
          match Hashtbl.find_opt light_beams (x_coord, y_coord) with
          | None ->
              Hashtbl.add light_beams (x_coord, y_coord) [from_direction] ;
              add_to_queue_and_next x_coord y_coord from_direction tile tail
          | Some directions ->
              let mem_checker =
                match tile with
                | '-' | '|' ->
                    fun () ->
                      List.mem from_direction directions
                      || List.mem
                           (get_opposite_direction from_direction)
                           directions
                | _ ->
                    fun () -> List.mem from_direction directions
              in
              if mem_checker () then simulate_light_beams tail
              else (
                Hashtbl.replace light_beams (x_coord, y_coord)
                  (from_direction :: directions) ;
                add_to_queue_and_next x_coord y_coord from_direction tile tail )
        else simulate_light_beams tail
    | _ ->
        ()
  in
  simulate_light_beams light_beam_queue ;
  light_beams |> Hashtbl.to_seq_keys

let part_1_aux path =
  Utils.file_to_grid path |> get_energized_tiles (0, 0) FromLeft |> Seq.length

let test_1 () =
  part_1_aux "lib/day16/test.txt" |> print_int ;
  print_newline ()

let part_1 () = part_1_aux "lib/day16/input.txt"

let part_2_aux path =
  let grid = Utils.file_to_grid path in
  let row_length = Array.length grid.(0) and col_length = Array.length grid in
  let rows =
    List.init (Array.length grid) (fun idx ->
        [ get_energized_tiles (0, idx) FromLeft grid |> Seq.length
        ; get_energized_tiles (row_length - 1, idx) FromRight grid |> Seq.length
        ] )
    |> List.flatten
  and columns =
    List.init
      (Array.length grid.(0))
      (fun idx ->
        [ get_energized_tiles (idx, 0) FromUp grid |> Seq.length
        ; get_energized_tiles (idx, col_length - 1) FromDown grid |> Seq.length
        ] )
    |> List.flatten
  in
  Utils.find_max (fun a b -> a - b) (rows @ columns)

let test_2 () =
  part_2_aux "lib/day16/test.txt" |> print_int ;
  print_newline ()

let part_2 () = part_2_aux "lib/day16/input.txt"

let solution () =
  print_string "Part 1: " ;
  part_1 () |> print_int ;
  print_newline () ;
  print_string "Part 2: " ;
  part_2 () |> print_int ;
  print_newline ()
