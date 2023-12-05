let extract_nums_and_symbols lines =
  let num_of_chars = List.length lines * (List.hd lines |> String.length) in
  let symbol_coords = Hashtbl.create num_of_chars in
  let nums =
    lines
    |> Utils.fold_lefti
         (fun acc line_idx line ->
           line |> String.to_seq |> List.of_seq
           |> Utils.fold_lefti
                (fun ((nums, numAcc) as acc) symbol_idx symbol ->
                  match (symbol, String.length numAcc) with
                  | '0' .. '9', _ ->
                      (nums, numAcc ^ String.make 1 symbol)
                  | '.', 0 ->
                      acc
                  | '.', _ ->
                      ( (numAcc, (line_idx, symbol_idx - String.length numAcc))
                        :: nums
                      , "" )
                  | _, 0 ->
                      Hashtbl.add symbol_coords (line_idx, symbol_idx) symbol ;
                      acc
                  | _, _ ->
                      Hashtbl.add symbol_coords (line_idx, symbol_idx) symbol ;
                      ( (numAcc, (line_idx, symbol_idx - String.length numAcc))
                        :: nums
                      , "" ) )
                (acc, "")
           |> fun (nums, numAcc) ->
           match numAcc with
           | "" ->
               nums
           | _ ->
               (numAcc, (line_idx, String.length line - String.length numAcc))
               :: nums )
         []
  in
  (nums, symbol_coords)

let part_1_aux path =
  let lines = Utils.file_to_list path in
  let nums, symbols = extract_nums_and_symbols lines
  and num_of_rows = List.length lines
  and num_of_cols = List.hd lines |> String.length in
  List.fold_left
    (fun sum (num_str, (row_coord, start_col_coord)) ->
      sum
      +
      if
        List.init (String.length num_str) (fun idx -> idx + start_col_coord)
        |> List.exists (fun col_coord ->
               Utils.get_neighbor_idxs (row_coord, col_coord) num_of_rows
                 num_of_cols
               |> List.exists (fun coord -> Hashtbl.mem symbols coord) )
      then int_of_string num_str
      else 0 )
    0 nums

let part_2_aux path =
  let lines = Utils.file_to_list path in
  let nums, symbols = extract_nums_and_symbols lines
  and num_of_rows = List.length lines
  and num_of_cols = List.hd lines |> String.length in
  let gear_to_nums = Hashtbl.create (num_of_rows * num_of_cols) in
  List.iter
    (fun (num_str, (row_coord, start_col_coord)) ->
      let _ =
        List.init (String.length num_str) (fun idx -> idx + start_col_coord)
        |> List.exists (fun col_coord ->
               Utils.get_neighbor_idxs (row_coord, col_coord) num_of_rows
                 num_of_cols
               |> List.exists (fun coord ->
                      if Hashtbl.find_opt symbols coord = Some '*' then (
                        let num = int_of_string num_str in
                        ( match Hashtbl.find_opt gear_to_nums coord with
                        | Some num_lst ->
                            Hashtbl.replace gear_to_nums coord (num :: num_lst)
                        | None ->
                            Hashtbl.add gear_to_nums coord [num] ) ;
                        true )
                      else false ) )
      in
      () )
    nums ;
  Hashtbl.to_seq_values gear_to_nums
  |> List.of_seq
  |> List.filter (fun num_lst -> List.length num_lst = 2)
  |> List.fold_left
       (fun sum num_lst ->
         match num_lst with
         | [num_1; num_2] ->
             sum + (num_1 * num_2)
         | _ ->
             failwith "This should not happen." )
       0

let test_1 () = part_1_aux "lib/day03/test.txt" |> print_int

let part_1 () = part_1_aux "lib/day03/input.txt"

let test_2 () = part_2_aux "lib/day03/test.txt" |> print_int

let part_2 () = part_2_aux "lib/day03/input.txt"

let solution () =
  print_string "Part 1: " ;
  part_1 () |> print_int ;
  print_newline () ;
  print_string "Part 2: " ;
  part_2 () |> print_int ;
  print_newline ()
