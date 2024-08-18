type reflection_line = Vertical of int | Horizontal of int

let find_reflection is_symmetrical rows =
  let columns = Utils.transpose rows in
  let rec find_reflection_aux idx lines =
    if idx = Array.length lines - 1 then None
    else if is_symmetrical idx lines then Some idx
    else find_reflection_aux (idx + 1) lines
  in
  match find_reflection_aux 0 rows with
  | Some symmetry_idx ->
      Horizontal symmetry_idx
  | None -> (
    match find_reflection_aux 0 columns with
    | Some symmetry_idx ->
        Vertical symmetry_idx
    | None ->
        failwith "Reflection line not found." )

let analyze_notes is_symmetrical path =
  Utils.file_to_string path
  |> Str.split (Str.regexp "\n\n")
  |> List.fold_left
       (fun sum pattern ->
         let pattern_lst = String.split_on_char '\n' pattern in
         let grid =
           Array.init (List.length pattern_lst) (fun _ ->
               Array.make (List.hd pattern_lst |> String.length) ' ' )
         in
         List.iteri
           (fun i line ->
             String.to_seq line
             |> Seq.iteri (fun j char -> grid.(i).(j) <- char) )
           pattern_lst ;
         sum
         +
         match find_reflection is_symmetrical grid with
         | Vertical to_left ->
             to_left + 1
         | Horizontal to_top ->
             100 * (to_top + 1) )
       0

let part_1_aux path =
  let is_symmetrical symmetry_idx lines =
    let rec is_symmetrical_aux idx =
      let idx_left = symmetry_idx - idx
      and idx_right = symmetry_idx + idx + 1 in
      if idx_left < 0 || idx_right >= Array.length lines then true
      else if lines.(idx_left) = lines.(idx_right) then
        is_symmetrical_aux (idx + 1)
      else false
    in
    is_symmetrical_aux 0
  in
  analyze_notes is_symmetrical path

let do_lines_differ_by_one line_1 line_2 =
  let rec do_lines_differ_by_one_aux idx already_differed =
    if idx >= Array.length line_1 then already_differed
    else
      let do_chars_differ = line_1.(idx) <> line_2.(idx) in
      match (already_differed, do_chars_differ) with
      | true, true ->
          false
      | false, true ->
          do_lines_differ_by_one_aux (idx + 1) true
      | _ ->
          do_lines_differ_by_one_aux (idx + 1) already_differed
  in
  do_lines_differ_by_one_aux 0 false

let part_2_aux path =
  let is_symmetrical symmetry_idx lines =
    let rec is_symmetrical_aux idx already_differed_by_one =
      let idx_left = symmetry_idx - idx
      and idx_right = symmetry_idx + idx + 1 in
      if idx_left < 0 || idx_right >= Array.length lines then
        already_differed_by_one
      else
        let do_differ_by_one =
          do_lines_differ_by_one lines.(idx_left) lines.(idx_right)
        in
        match (do_differ_by_one, already_differed_by_one) with
        | true, true ->
            false
        | true, false ->
            is_symmetrical_aux (idx + 1) true
        | false, _ ->
            if lines.(idx_left) = lines.(idx_right) then
              is_symmetrical_aux (idx + 1) already_differed_by_one
            else false
    in
    is_symmetrical_aux 0 false
  in
  analyze_notes is_symmetrical path

let test_1 () =
  part_1_aux "lib/day13/test.txt" |> print_int ;
  print_newline ()

let part_1 () = part_1_aux "lib/day13/input.txt"

let test_2 () =
  part_2_aux "lib/day13/test.txt" |> print_int ;
  print_newline ()

let part_2 () = part_2_aux "lib/day13/input.txt"

let solution () =
  print_endline "DAY 13" ;
  print_string "Part 1: " ;
  part_1 () |> print_int ;
  print_newline () ;
  print_string "Part 2: " ;
  part_2 () |> print_int ;
  print_newline ()
