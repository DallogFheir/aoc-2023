module Node = struct
  type t =
    int
    * (int * int)
    * int
    * Direction.direction option
    * ((int * int) * int * Direction.direction option) option

  let compare a b = if a = b then 0 else if a > b then 1 else -1
end

module PriorityMap = Map.Make (Node)

let get_neighbors (x_coord, y_coord) count_in_straight_line from_direction grid
    steer_fn =
  let to_add =
    [ ((-1, 0), Direction.FromLeft)
    ; ((0, -1), Direction.FromTop)
    ; ((1, 0), Direction.FromRight)
    ; ((0, 1), Direction.FromBottom) ]
    |> steer_fn count_in_straight_line from_direction
  and row_length = Array.length grid.(0)
  and col_length = Array.length grid in
  to_add
  |> List.map (fun ((to_add_x, to_add_y), direction) ->
         ((x_coord + to_add_x, y_coord + to_add_y), direction) )
  |> List.filter (fun ((neighbor_x_coord, neighbor_y_coord), _) ->
         neighbor_x_coord >= 0
         && neighbor_x_coord < row_length
         && neighbor_y_coord >= 0
         && neighbor_y_coord < col_length )

let solution_aux path steer_fn count_in_straight_line_fn =
  let queue =
    PriorityMap.empty |> PriorityMap.add (0, (0, 0), 0, None, None) true
  and grid = Utils.file_to_grid path in
  let row_length = Array.length grid.(0) and col_length = Array.length grid in
  let target_coords = (row_length - 1, col_length - 1)
  and visited = Hashtbl.create (row_length * col_length) in
  let rec find_shortest_path queue =
    if PriorityMap.is_empty queue then ()
    else
      let ( ( (path_length, coords, count_in_straight_line, from_direction, prev)
              as node )
          , _ ) =
        PriorityMap.min_binding queue
      in
      if Hashtbl.mem visited (coords, count_in_straight_line, from_direction)
      then find_shortest_path (PriorityMap.remove node queue)
      else (
        Hashtbl.replace visited
          (coords, count_in_straight_line, from_direction)
          (path_length, prev) ;
        get_neighbors coords count_in_straight_line from_direction grid steer_fn
        |> List.fold_left
             (fun new_queue
                  ( ((neighbor_x_coord, neighbor_y_coord) as neighbor_coords)
                  , neighbor_direction ) ->
               let dist_to_neighbor =
                 Utils.digit_char_to_number
                   grid.(neighbor_y_coord).(neighbor_x_coord)
               in
               PriorityMap.add
                 ( path_length + dist_to_neighbor
                 , neighbor_coords
                 , ( match from_direction with
                   | Some dir when dir = neighbor_direction ->
                       count_in_straight_line + 1
                   | _ ->
                       0 )
                 , Some neighbor_direction
                 , Some (coords, count_in_straight_line, from_direction) )
                 true new_queue )
             (PriorityMap.remove node queue)
        |> find_shortest_path )
  in
  find_shortest_path queue ;
  let _, (path_length, _) =
    Hashtbl.to_seq visited |> List.of_seq
    |> List.find_all (fun ((coords, count_in_straight_line, _), _) ->
           coords = target_coords
           && count_in_straight_line_fn count_in_straight_line )
    |> Utils.find_max (fun (_, (path_length_a, _)) (_, (path_length_b, _)) ->
           path_length_b - path_length_a )
  in
  path_length

let part_1_aux path =
  solution_aux path
    (fun count_in_straight_line from_direction ->
      match (count_in_straight_line, from_direction) with
      | 2, Some dir ->
          List.filter (fun (_, direction) -> direction <> dir)
      | _, Some dir ->
          List.filter (fun (_, direction) ->
              direction <> Direction.get_opposite_direction dir )
      | _ ->
          fun x -> x )
    (fun _ -> true)

let test_1 () =
  part_1_aux "lib/day17/test1.txt" |> print_int ;
  print_newline ()

let part_1 () = part_1_aux "lib/day17/input.txt"

let part_2_aux path =
  solution_aux path
    (fun count_in_straight_line from_direction ->
      if count_in_straight_line < 3 then
        List.filter (fun (_, direction) ->
            match from_direction with
            | None ->
                true
            | Some dir ->
                dir = direction )
      else
        match (count_in_straight_line, from_direction) with
        | 9, Some dir ->
            List.filter (fun (_, direction) -> direction <> dir)
        | _, Some dir ->
            List.filter (fun (_, direction) ->
                direction <> Direction.get_opposite_direction dir )
        | _ ->
            fun x -> x )
    (fun count_in_straight_line -> count_in_straight_line >= 3)

let test_2 () =
  part_2_aux "lib/day17/test1.txt" |> print_int ;
  print_newline () ;
  part_2_aux "lib/day17/test2.txt" |> print_int ;
  print_newline ()

let part_2 () = part_2_aux "lib/day17/input.txt"

let solution () =
  print_string "Part 1: " ;
  part_1 () |> print_int ;
  print_newline () ;
  print_string "Part 2: " ;
  part_2 () |> print_int ;
  print_newline ()
