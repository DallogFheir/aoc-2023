let parse_input input =
  match Str.split (Str.regexp "\n\n") input with
  | [instructions; nodes_str] ->
      let node_lines = String.split_on_char '\n' nodes_str in
      let nodes = Hashtbl.create (List.length node_lines) in
      node_lines
      |> List.iter (fun line ->
             if
               Str.string_match
                 (Str.regexp {|\(.+\) = (\(.+\), \(.+\))|})
                 line 0
             then
               let node = Str.matched_group 1 line
               and left = Str.matched_group 2 line
               and right = Str.matched_group 3 line in
               Hashtbl.add nodes node (left, right)
             else failwith "Node line did not match pattern." ) ;
      (instructions |> String.to_seq |> List.of_seq, nodes)
  | _ ->
      failwith "Invalid input."

let rec count_steps_to_end_node is_end_node current_node current_instructions
    all_instructions nodes steps =
  if is_end_node current_node then steps
  else
    let instruction, next_instructions =
      match
        if current_instructions = [] then all_instructions
        else current_instructions
      with
      | instruction :: next_instructions ->
          (instruction, next_instructions)
      | _ ->
          failwith "Instructions are empty."
    in
    let next_node =
      let left, right = Hashtbl.find nodes current_node in
      match instruction with
      | 'L' ->
          left
      | 'R' ->
          right
      | _ ->
          failwith ("Invalid instruction: " ^ String.make 1 instruction)
    in
    count_steps_to_end_node is_end_node next_node next_instructions
      all_instructions nodes (steps + 1)

let part_1_aux path =
  let instructions, nodes = Utils.file_to_string path |> parse_input in
  count_steps_to_end_node
    (fun node -> node = "ZZZ")
    "AAA" instructions instructions nodes 0

let factorize n =
  let rec factorize_aux n factor factors =
    if n = 1 then factors
    else if n mod factor = 0 then (
      ( match Hashtbl.find_opt factors factor with
      | Some count ->
          Hashtbl.replace factors factor (count + 1)
      | None ->
          Hashtbl.add factors factor 1 ) ;
      factorize_aux (n / factor) factor factors )
    else factorize_aux n (factor + 1) factors
  in
  factorize_aux n 2 (Hashtbl.create (n |> float_of_int |> sqrt |> int_of_float))

let part_2_aux path =
  let instructions, nodes = Utils.file_to_string path |> parse_input in
  let factorized_lst =
    List.filter
      (fun (node, _) -> node.[String.length node - 1] = 'A')
      (nodes |> Hashtbl.to_seq |> List.of_seq)
    |> List.map (fun (node, _) ->
           count_steps_to_end_node
             (fun potential_end_node ->
               potential_end_node.[String.length potential_end_node - 1] = 'Z'
               )
             node instructions instructions nodes 0 )
    |> List.map factorize
  and final_factors = Hashtbl.create 10 in
  List.iter
    (fun factors ->
      Hashtbl.to_seq factors
      |> Seq.iter (fun (factor, count) ->
             match Hashtbl.find_opt final_factors factor with
             | Some already_count ->
                 Hashtbl.replace final_factors factor (max count already_count)
             | None ->
                 Hashtbl.add final_factors factor count ) )
    factorized_lst ;
  Hashtbl.to_seq final_factors
  |> Seq.fold_left
       (fun lcm (factor, count) ->
         lcm * (float_of_int factor ** float_of_int count |> int_of_float) )
       1

let test_1 () = part_1_aux "lib/day08/test.txt" |> print_int

let part_1 () = part_1_aux "lib/day08/input.txt"

let test_2 () = part_2_aux "lib/day08/test.txt" |> print_int

let part_2 () = part_2_aux "lib/day08/input.txt"

let solution () =
  print_string "Part 1: " ;
  part_1 () |> print_int ;
  print_newline () ;
  print_string "Part 2: " ;
  part_2 () |> print_int ;
  print_newline ()
