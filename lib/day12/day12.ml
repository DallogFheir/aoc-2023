type spring_group = Damaged of int | Unknown of int | Operational

let get_spring_groups spring_str =
  spring_str |> String.to_seq |> List.of_seq |> Utils.groupby
  |> List.map (fun (spring, count) ->
         match spring with
         | '#' ->
             Damaged count
         | '.' ->
             Operational
         | '?' ->
             Unknown count
         | _ ->
             failwith ("Invalid spring: " ^ String.make 1 spring) )

let calculate_combinations springs damaged_counts =
  let cache = Hashtbl.create 50 in
  let rec calculate_combinations_aux springs damaged_counts to_borrow =
    let res =
      match Hashtbl.find_opt cache (springs, damaged_counts, to_borrow) with
      | None -> (
        match (springs, damaged_counts) with
        | _, [] ->
            if
              List.for_all
                (fun spring ->
                  match spring with Damaged _ -> false | _ -> true )
                springs
            then 1
            else 0
        | [], _ ->
            0
        | group :: group_tail, count :: count_tail -> (
          match group with
          | Operational ->
              if to_borrow = 0 then
                calculate_combinations_aux group_tail damaged_counts 0
              else 0
          | Unknown unknown_count ->
              if to_borrow = 0 then
                if unknown_count >= count then
                  match group_tail with
                  | [] | Operational :: _ ->
                      let possibilities = unknown_count - count + 1 in
                      calculate_combinations_aux group_tail damaged_counts 0
                      + ( List.init possibilities (fun idx -> idx)
                        |> List.fold_left
                             (fun sum i ->
                               let left =
                                 max 0 (unknown_count - count - 1 - i)
                               in
                               sum
                               + calculate_combinations_aux
                                   ( if left = 0 then group_tail
                                     else Unknown left :: group_tail )
                                   count_tail 0 )
                             0 )
                  | Damaged next_damaged_count :: next_tail ->
                      let possibilities = unknown_count - count in
                      ( List.init unknown_count (fun idx -> idx + 1)
                      |> List.fold_left
                           (fun sum i ->
                             sum
                             + calculate_combinations_aux
                                 (Damaged (next_damaged_count + i) :: next_tail)
                                 damaged_counts 0 )
                           0 )
                      + calculate_combinations_aux group_tail damaged_counts 0
                      + ( List.init possibilities (fun idx -> idx)
                        |> List.fold_left
                             (fun sum i ->
                               let left =
                                 max 0 (unknown_count - count - 1 - i)
                               in
                               sum
                               + calculate_combinations_aux
                                   ( if left = 0 then group_tail
                                     else Unknown left :: group_tail )
                                   count_tail 0 )
                             0 )
                  | Unknown _ :: _ ->
                      failwith "Two Unknown in a row."
                else
                  match group_tail with
                  | [] | Operational :: _ ->
                      calculate_combinations_aux group_tail damaged_counts 0
                  | Damaged next_damaged_count :: next_tail ->
                      let possibilities = unknown_count in
                      List.init (possibilities + 1) (fun idx -> idx)
                      |> List.fold_left
                           (fun sum i ->
                             let to_add = unknown_count - i in
                             sum
                             + calculate_combinations_aux
                                 ( Damaged (next_damaged_count + to_add)
                                 :: next_tail )
                                 damaged_counts 0 )
                           0
                  | Unknown _ :: _ ->
                      failwith "Two Unknown in a row."
              else if unknown_count < to_borrow then
                match group_tail with
                | [] | Operational :: _ ->
                    0
                | Damaged _ :: _ ->
                    calculate_combinations_aux group_tail damaged_counts
                      (to_borrow - unknown_count)
                | Unknown _ :: _ ->
                    failwith "Two Unknown in a row."
              else if unknown_count = to_borrow then
                match group_tail with
                | [] | Operational :: _ ->
                    calculate_combinations_aux group_tail count_tail 0
                | Damaged _ :: _ ->
                    0
                | Unknown _ :: _ ->
                    failwith "Two Unknown in a row."
              else
                let left = max 0 (unknown_count - to_borrow - 1) in
                calculate_combinations_aux
                  (if left = 0 then group_tail else Unknown left :: group_tail)
                  count_tail 0
          | Damaged damaged_count ->
              if to_borrow = 0 then
                if damaged_count > count then 0
                else if damaged_count = count then
                  match group_tail with
                  | [] | Operational :: _ ->
                      calculate_combinations_aux group_tail count_tail 0
                  | Unknown next_unknown_count :: next_tail ->
                      calculate_combinations_aux
                        ( if next_unknown_count = 1 then next_tail
                          else Unknown (next_unknown_count - 1) :: next_tail )
                        count_tail 0
                  | Damaged _ :: _ ->
                      failwith "Two Damaged in a row."
                else
                  calculate_combinations_aux group_tail damaged_counts
                    (count - damaged_count)
              else if damaged_count > to_borrow then 0
              else if damaged_count = to_borrow then
                match group_tail with
                | [] | Operational :: _ ->
                    calculate_combinations_aux group_tail count_tail 0
                | Unknown next_unknown_count :: next_tail ->
                    calculate_combinations_aux
                      ( if next_unknown_count = 1 then next_tail
                        else Unknown (next_unknown_count - 1) :: next_tail )
                      count_tail 0
                | Damaged _ :: _ ->
                    failwith "Two Damaged in a row."
              else
                calculate_combinations_aux group_tail damaged_counts
                  (to_borrow - damaged_count) ) )
      | Some v ->
          v
    in
    Hashtbl.add cache (springs, damaged_counts, to_borrow) res ;
    res
  in
  calculate_combinations_aux springs damaged_counts 0

let part_1_aux path =
  Utils.file_to_list path
  |> Utils.fold_lefti
       (fun sum idx line ->
         match String.split_on_char ' ' line with
         | [springs; damaged_counts] ->
             sum
             + calculate_combinations
                 (get_spring_groups springs)
                 ( String.split_on_char ',' damaged_counts
                 |> List.map (fun num -> int_of_string num) )
         | _ ->
             failwith ("Invalid line: " ^ line) )
       0

let test_1 () =
  part_1_aux "lib/day12/test.txt" |> print_int ;
  print_newline ()

let part_1 () = part_1_aux "lib/day12/input.txt"

(* let test_2 () =
     part_2_aux "lib/day12/test.txt" 10 |> print_int ;
     print_newline () ;
     part_2_aux "lib/day12/test.txt" 100 |> print_int ;
     print_newline ()

   let part_2 () = part_2_aux "lib/day12/input.txt" 1_000_000 *)

let solution () =
  print_string "Part 1: " ;
  part_1 () |> print_int ;
  print_newline ()
(*;
  print_string "Part 2: " ;
  part_2 () |> print_int ;
  print_newline () *)
