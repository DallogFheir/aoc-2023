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

let parse_hailstones path =
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

let part_1_aux path test_area_min test_area_max =
  parse_hailstones path |> find_how_many_intersect test_area_min test_area_max

let dot_product v1 v2 =
  if Array.length v1 <> Array.length v2 then
    failwith "Dot product of vectors of different dimensions." ;
  let rec dot_product_aux idx res =
    if idx = Array.length v1 then res
    else dot_product_aux (idx + 1) (res +. (v1.(idx) *. v2.(idx)))
  in
  dot_product_aux 0 0.

let cross_product v1 v2 =
  if Array.length v1 <> 3 || Array.length v2 <> 3 then
    failwith "Cross product supported only for 3D vectors." ;
  let i = (v1.(1) *. v2.(2)) -. (v1.(2) *. v2.(1))
  and j = (v1.(2) *. v2.(0)) -. (v1.(0) *. v2.(2))
  and k = (v1.(0) *. v2.(1)) -. (v1.(1) *. v2.(0)) in
  [|i; j; k|]

let vector_by_scalar_aux v sc op =
  let res = Array.make (Array.length v) 0. in
  let rec vector_by_scalar_aux_aux idx =
    if idx = Array.length res then res
    else (
      res.(idx) <- op v.(idx) sc ;
      vector_by_scalar_aux_aux (idx + 1) )
  in
  vector_by_scalar_aux_aux 0

let vector_by_scalar_mul sc v = vector_by_scalar_aux v sc ( *. )

let vector_by_scalar_div v sc =
  if sc = 0. then failwith "Division by 0." ;
  vector_by_scalar_aux v sc ( /. )

let vector_add v1 v2 =
  if Array.length v1 <> Array.length v2 then
    failwith "Sum of vectors of different dimensions." ;
  let res = Array.make (Array.length v1) 0. in
  let rec vector_diff_aux idx =
    if idx = Array.length res then res
    else (
      res.(idx) <- v1.(idx) +. v2.(idx) ;
      vector_diff_aux (idx + 1) )
  in
  vector_diff_aux 0

let vector_diff v1 v2 =
  if Array.length v1 <> Array.length v2 then
    failwith "Difference of vectors of different dimensions." ;
  vector_add v1 (vector_by_scalar_mul (-1.) v2)

let test_1 () =
  part_1_aux "lib/day24/test.txt" 7. 27. |> print_int ;
  print_newline ()

let part_1 () =
  part_1_aux "lib/day24/input.txt" 200000000000000. 400000000000000.

let part_2_aux path =
  let hailstones = parse_hailstones path in
  match hailstones with
  | ((px_0, py_0, pz_0), (vx_0, vy_0, vz_0), _)
    :: ((px_1, py_1, pz_1), (vx_1, vy_1, vz_1), _)
    :: ((px_2, py_2, pz_2), (vx_2, vy_2, vz_2), _)
    :: _ ->
      let pos_0 = [|px_0; py_0; pz_0|]
      and pos_1 = [|px_1; py_1; pz_1|]
      and pos_2 = [|px_2; py_2; pz_2|]
      and vel_0 = [|vx_0; vy_0; vz_0|]
      and vel_1 = [|vx_1; vy_1; vz_1|]
      and vel_2 = [|vx_2; vy_2; vz_2|] in
      let p1 = vector_diff pos_1 pos_0
      and v1 = vector_diff vel_1 vel_0
      and p2 = vector_diff pos_2 pos_0
      and v2 = vector_diff vel_2 vel_0 in
      let t1 =
        -.dot_product (cross_product p1 p2) v2
        /. dot_product (cross_product v1 p2) v2
      and t2 =
        -.dot_product (cross_product p1 p2) v1
        /. dot_product (cross_product p1 v2) v1
      in
      let c1 = vector_add pos_1 (vector_by_scalar_mul t1 vel_1)
      and c2 = vector_add pos_2 (vector_by_scalar_mul t2 vel_2) in
      let v = vector_by_scalar_div (vector_diff c2 c1) (t2 -. t1) in
      let p = vector_diff c1 (vector_by_scalar_mul t1 v) in
      p.(0) +. p.(1) +. p.(2) |> int_of_float
  | _ ->
      failwith "Input with less than 3 hailstones."

let test_2 () =
  part_2_aux "lib/day24/test.txt" |> print_int ;
  print_newline ()

let part_2 () = part_2_aux "lib/day24/input.txt"

let solution () =
  print_endline "DAY 24:" ;
  print_string "Part 1: " ;
  part_1 () |> print_int ;
  print_newline () ;
  print_string "Part 2: " ;
  part_2 () |> print_int ;
  print_newline ()
