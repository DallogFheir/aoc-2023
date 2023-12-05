let map_to_num_of_wins cards =
  let pattern = Str.regexp {|^Card +[0-9]+: +\(.+\) | \(.+\)$|} in
  cards
  |> List.map (fun line ->
         if Str.string_match pattern line 0 then (
           let winning_nums = Hashtbl.create (List.length cards) in
           Str.matched_group 1 line |> String.split_on_char ' '
           |> List.iter (fun num ->
                  if num <> "" then Hashtbl.add winning_nums num () ) ;
           let our_nums =
             Str.matched_group 2 line |> String.split_on_char ' '
             |> List.filter (fun substr -> substr <> "")
           in
           let count =
             List.fold_left
               (fun count our_num ->
                 count + if Hashtbl.mem winning_nums our_num then 1 else 0 )
               0 our_nums
           in
           count )
         else failwith ("Pattern did not match line " ^ line ^ ".") )

let part_1_aux path =
  Common.file_to_list path |> map_to_num_of_wins
  |> List.fold_left
       (fun sum num_of_wins ->
         sum
         +
         if num_of_wins = 0 then 0
         else float_of_int 2 ** float_of_int (num_of_wins - 1) |> int_of_float
         )
       0

let part_2_aux path =
  let lines = Common.file_to_list path in
  lines |> map_to_num_of_wins
  |> List.fold_left
       (fun (count, prev) num_of_wins ->
         match prev with
         | head :: tail ->
             ( count + head
             , Common.map2_shortest
                 (fun a b -> a + b)
                 (List.init num_of_wins (fun _ -> head))
                 tail )
         | _ ->
             (count, []) )
       (0, List.init (List.length lines) (fun _ -> 1))
  |> fst

let test_1 () = part_1_aux "lib/day04/test.txt" |> print_int

let part_1 () = part_1_aux "lib/day04/input.txt"

let test_2 () = part_2_aux "lib/day04/test.txt" |> print_int

let part_2 () = part_2_aux "lib/day04/input.txt"

let solution () =
  print_string "Part 1: " ;
  part_1 () |> print_int ;
  print_newline () ;
  print_string "Part 2: " ;
  part_2 () |> print_int ;
  print_newline ()
