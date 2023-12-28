open Rangedict

let get_vertical_expansion_ranges grid row_length col_length =
  let prev_start, count, rd =
    List.init row_length (fun _ -> ())
    |> Utils.fold_lefti
         (fun (prev_start, count, rd) i _ ->
           if
             Utils.loop_with_break
               (fun j -> grid.(j).(i) = '#')
               0 (col_length - 1)
           then (prev_start, count, rd)
           else (i, count + 1, RangeDict.add (prev_start + 1, i) count rd) )
         (-1, 0, RangeDict.empty)
  in
  RangeDict.add (prev_start + 1, row_length) count rd

let get_galaxies expansion_multiplier grid =
  let row_length = Array.length grid.(0) and col_length = Array.length grid in
  let vertical_expansion_ranges =
    get_vertical_expansion_ranges grid row_length col_length
  in
  List.init col_length (fun _ -> ())
  |> Utils.fold_lefti
       (fun (horizontal_expansion_count, galaxies) i _ ->
         let was_there_galaxy, new_galaxies =
           List.init row_length (fun _ -> ())
           |> Utils.fold_lefti
                (fun (was_there_galaxy, galaxies) j _ ->
                  if grid.(i).(j) = '#' then
                    match RangeDict.get j vertical_expansion_ranges with
                    | Some to_add_vertically ->
                        ( true
                        , ( i
                            + ( expansion_multiplier
                                * horizontal_expansion_count
                              - horizontal_expansion_count )
                          , j
                            + ( (expansion_multiplier * to_add_vertically)
                              - to_add_vertically ) )
                          :: galaxies )
                    | None ->
                        failwith
                          ( string_of_int j
                          ^ " not found in vertical expansion ranges." )
                  else (was_there_galaxy, galaxies) )
                (false, galaxies)
         in
         ( (horizontal_expansion_count + if was_there_galaxy then 0 else 1)
         , new_galaxies ) )
       (0, [])
  |> snd

let get_manhattan_distance (i_1, j_1) (i_2, j_2) =
  abs (i_1 - i_2) + abs (j_1 - j_2)

let calculate_sum_of_distances nodes =
  let rec calculate_sum_of_distances_aux sum nodes =
    match nodes with
    | [] ->
        sum
    | node :: tail ->
        calculate_sum_of_distances_aux
          (List.fold_left
             (fun sum other_node -> sum + get_manhattan_distance node other_node)
             sum tail )
          tail
  in
  calculate_sum_of_distances_aux 0 nodes

let part_1_aux path =
  Utils.file_to_grid path |> get_galaxies 2 |> calculate_sum_of_distances

let part_2_aux path multiplier =
  Utils.file_to_grid path |> get_galaxies multiplier
  |> calculate_sum_of_distances

let test_1 () =
  part_1_aux "lib/day11/test.txt" |> print_int ;
  print_newline ()

let part_1 () = part_1_aux "lib/day11/input.txt"

let test_2 () =
  part_2_aux "lib/day11/test.txt" 10 |> print_int ;
  print_newline () ;
  part_2_aux "lib/day11/test.txt" 100 |> print_int ;
  print_newline ()

let part_2 () = part_2_aux "lib/day11/input.txt" 1_000_000

let solution () =
  print_endline "DAY 11" ;
  print_string "Part 1: " ;
  part_1 () |> print_int ;
  print_newline () ;
  print_string "Part 2: " ;
  part_2 () |> print_int ;
  print_newline ()
