let parse_bricks bricks_lst =
  let bricks = Hashtbl.create (List.length bricks_lst) in
  List.iteri
    (fun brick_idx brick_str ->
      match String.split_on_char '~' brick_str with
      | [brick_start; brick_end] -> (
        match
          ( String.split_on_char ',' brick_start
          , String.split_on_char ',' brick_end )
        with
        | [start_x; start_y; start_z], [end_x; end_y; end_z] ->
            Utils.loop
              (fun i ->
                Utils.loop
                  (fun j ->
                    Utils.loop
                      (fun k -> Hashtbl.replace bricks (i, j, k) brick_idx)
                      (int_of_string start_z) (int_of_string end_z) )
                  (int_of_string start_y) (int_of_string end_y) )
              (int_of_string start_x) (int_of_string end_x)
        | _ ->
            failwith ("Invalid brick: " ^ brick_str) )
      | _ ->
          failwith ("Invalid brick :" ^ brick_str) )
    bricks_lst ;
  bricks

let simulate_fall bricks =
  let rec simulate_fall_aux bricks =
    let _, _, highest_height =
      Hashtbl.to_seq_keys bricks |> List.of_seq
      |> Utils.find_max (fun (_, _, z1) (_, _, z2) ->
             if z1 > z2 then 1 else if z1 = z2 then 0 else -1 )
    and new_bricks = Hashtbl.copy bricks in
    let did_fall =
      Array.make highest_height None
      |> Array.to_list
      |> Utils.fold_lefti
           (fun acc idx _ ->
             let height = idx + 1 in
             let bricks_at_height =
               Hashtbl.to_seq bricks
               |> Seq.filter (fun ((_, _, z), _) -> z = height)
               |> Utils.groupby (fun (_, brick_idx) -> brick_idx)
             in
             Hashtbl.to_seq_values bricks_at_height
             |> Seq.fold_left
                  (fun acc brick_coords ->
                    if
                      List.for_all
                        (fun ((x, y, z), _) ->
                          let lower_coords = (x, y, z - 1) in
                          z - 1 <> 0 && not (Hashtbl.mem bricks lower_coords) )
                        brick_coords
                    then (
                      List.iter
                        (fun (coords, _) -> Hashtbl.remove new_bricks coords)
                        brick_coords ;
                      List.iter
                        (fun ((x, y, z), brick_idx) ->
                          Hashtbl.replace new_bricks (x, y, z - 1) brick_idx )
                        brick_coords ;
                      true )
                    else acc )
                  false
             || acc )
           false
    in
    if did_fall then simulate_fall_aux new_bricks else new_bricks
  in
  simulate_fall_aux bricks

let find_safe_to_disintegrate bricks =
  let safe_to_disintegrate = Hashtbl.create (Hashtbl.length bricks)
  and coords_by_brick =
    Hashtbl.to_seq bricks |> Utils.groupby (fun (_, brick_idx) -> brick_idx)
  in
  coords_by_brick |> Hashtbl.to_seq
  |> Seq.iter (fun (brick_idx, brick_lst) ->
         let supported_bricks = Hashtbl.create (List.length brick_lst) in
         List.iter
           (fun ((x, y, z), _) ->
             match Hashtbl.find_opt bricks (x, y, z + 1) with
             | Some supported_brick when supported_brick <> brick_idx ->
                 Hashtbl.replace supported_bricks (supported_brick, z + 1) true
             | _ ->
                 () )
           brick_lst ;
         if
           Hashtbl.to_seq_keys supported_bricks
           |> Seq.for_all (fun (supported_brick, height) ->
                  not
                    ( Hashtbl.find coords_by_brick supported_brick
                    |> List.filter (fun ((_, _, z), _) -> z = height)
                    |> List.for_all (fun ((x, y, z), _) ->
                           match Hashtbl.find_opt bricks (x, y, z - 1) with
                           | Some supporting_brick ->
                               supporting_brick = brick_idx
                           | None ->
                               true ) ) )
         then Hashtbl.replace safe_to_disintegrate brick_idx true ) ;
  safe_to_disintegrate

let part_1_aux path =
  let bricks =
    Utils.file_to_list path |> parse_bricks |> simulate_fall
    |> find_safe_to_disintegrate
  in
  Hashtbl.length bricks

let test_1 () =
  part_1_aux "lib/day22/test.txt" |> print_int ;
  print_newline ()

let part_1 () = part_1_aux "lib/day22/input.txt"

let solution () =
  print_string "Part 1: " ;
  part_1 () |> print_int
(* print_newline () ;
   print_string "Part 2: " ;
   part_2 () |> print_int ;
   print_newline () *)
