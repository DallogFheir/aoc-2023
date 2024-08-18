let predict_next_value nums =
  let rec predict_next_value_aux nums last_values_sum =
    if List.for_all (fun num -> num = 0) nums then last_values_sum
    else
      let differences, last_value =
        List.fold_left
          (fun (differences, prev) current ->
            ( ( match prev with
              | None ->
                  differences
              | Some prev ->
                  (current - prev) :: differences )
            , Some current ) )
          ([], None) nums
      in
      match last_value with
      | None ->
          failwith "Empty numbers list."
      | Some last_value ->
          predict_next_value_aux (List.rev differences)
            (last_values_sum + last_value)
  in
  predict_next_value_aux nums 0

let predict_previous_value nums =
  let rec predict_previous_value_aux nums last_values =
    if List.for_all (fun num -> num = 0) nums then
      List.fold_left (fun difference num -> num - difference) 0 last_values
    else
      let differences, _ =
        List.fold_left
          (fun (differences, prev) current ->
            ( ( match prev with
              | None ->
                  differences
              | Some prev ->
                  (current - prev) :: differences )
            , Some current ) )
          ([], None) nums
      in
      predict_previous_value_aux (List.rev differences)
        (List.hd nums :: last_values)
  in
  predict_previous_value_aux nums []

let solve fn path =
  Utils.file_to_list path
  |> List.fold_left
       (fun sum line ->
         sum
         + ( line |> String.split_on_char ' '
           |> List.map (fun num -> int_of_string num)
           |> fn ) )
       0

let part_1_aux path = solve predict_next_value path

let part_2_aux path = solve predict_previous_value path

let test_1 () =
  part_1_aux "lib/day09/test.txt" |> print_int ;
  print_newline ()

let part_1 () = part_1_aux "lib/day09/input.txt"

let test_2 () =
  part_2_aux "lib/day09/test.txt" |> print_int ;
  print_newline ()

let part_2 () = part_2_aux "lib/day09/input.txt"

let solution () =
  print_endline "DAY 9" ;
  print_string "Part 1: " ;
  part_1 () |> print_int ;
  print_newline () ;
  print_string "Part 2: " ;
  part_2 () |> print_int ;
  print_newline ()
