let get_linear_function (x, y, _) (vx, vy, _) =
  let slope = vy /. vx in
  let b = y -. (slope *. x) in
  (slope, b)

let get_intersection_point (slope_1, b_1) (slope_2, b_2) =
  if slope_1 = slope_2 then None
  else
    let x_intersect = (b_2 -. b_1) /. (slope_1 -. slope_2) in
    let y_intersect = (slope_1 *. x_intersect) +. b_1 in
    Some (x_intersect, y_intersect)

let check_if_in_past coord_to_check coord velocity =
  let op =
    if velocity < 0. then ( > )
    else if velocity > 0. then ( < )
    else failwith "Velocity is 0."
  in
  op coord_to_check coord

let find_how_many_intersect test_area_min test_area_max hailstones =
  let rec find_how_many_intersect_aux hailstones total_count =
    match hailstones with
    | [] ->
        total_count
    | ((x_1, y_1, _), (vx_1, vy_1, _), fn_1) :: tail ->
        find_how_many_intersect_aux tail
          ( total_count
          + Utils.list_count_if
              (fun ((x_2, y_2, _), (vx_2, vy_2, _), fn_2) ->
                match get_intersection_point fn_1 fn_2 with
                | Some (x_intersect, y_intersect) ->
                    (not (check_if_in_past x_intersect x_1 vx_1))
                    && (not (check_if_in_past x_intersect x_2 vx_2))
                    && (not (check_if_in_past y_intersect y_1 vy_1))
                    && (not (check_if_in_past y_intersect y_2 vy_2))
                    && x_intersect >= test_area_min
                    && x_intersect <= test_area_max
                    && y_intersect >= test_area_min
                    && y_intersect <= test_area_max
                | None ->
                    false )
              tail )
  in
  find_how_many_intersect_aux hailstones 0

let part_1_aux path test_area_min test_area_max =
  Utils.file_to_list path
  |> List.map (fun line ->
         match Str.split (Str.regexp {| @ |}) line with
         | [coords_str; velocities_str] ->
             let coords =
               match
                 Str.split (Str.regexp {|, |}) coords_str
                 |> List.map (fun coord -> float_of_string (String.trim coord))
               with
               | [x; y; z] ->
                   (x, y, z)
               | _ ->
                   failwith ("Invalid coordinates: " ^ coords_str)
             and velocities =
               match
                 Str.split (Str.regexp {|, |}) velocities_str
                 |> List.map (fun velocity ->
                        float_of_string (String.trim velocity) )
               with
               | [vx; vy; vz] ->
                   (vx, vy, vz)
               | _ ->
                   failwith ("Invalid velocities: " ^ velocities_str)
             in
             (coords, velocities, get_linear_function coords velocities)
         | _ ->
             failwith ("Invalid hailstone info: " ^ line) )
  |> find_how_many_intersect test_area_min test_area_max

let test_1 () =
  part_1_aux "lib/day24/test.txt" 7. 27. |> print_int ;
  print_newline ()

let part_1 () =
  part_1_aux "lib/day24/input.txt" 200000000000000. 400000000000000.

(*
   let test_2 () =
     part_2_aux "lib/day24/test.txt" |> print_int ;
     print_newline ()

   let part_2 () = part_2_aux "lib/day24/input.txt" *)

let solution () =
  print_string "Part 1: " ;
  part_1 () |> print_int ;
  print_newline ()
(* ;
   print_string "Part 2: " ;
   part_2 () |> print_int ;
   print_newline () *)
