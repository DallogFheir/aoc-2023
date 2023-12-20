type comparison =
  | LessThan of string * int
  | GreaterThan of string * int
  | AlwaysTrue

type workflow_result = SendTo of string | Accept | Reject

let map_to_workflow_result str =
  match str with "A" -> Accept | "R" -> Reject | _ -> SendTo str

let is_part_accepted workflows (x, m, a, s) =
  let start_workflow = Hashtbl.find workflows "in"
  and select_category category_str =
    match category_str with
    | "x" ->
        x
    | "m" ->
        m
    | "a" ->
        a
    | "s" ->
        s
    | _ ->
        failwith ("Invalid category: " ^ category_str)
  in
  let rec execute_workflow rules =
    let rule, result = List.hd rules in
    match rule with
    | LessThan (category, value) when select_category category < value ->
        select_result result
    | GreaterThan (category, value) when select_category category > value ->
        select_result result
    | AlwaysTrue ->
        select_result result
    | _ ->
        execute_workflow (List.tl rules)
  and select_result result =
    match result with
    | Accept ->
        true
    | Reject ->
        false
    | SendTo workflow_name ->
        execute_workflow (Hashtbl.find workflows workflow_name)
  in
  execute_workflow start_workflow

let perform_workflows workflows parts =
  let accepted = Hashtbl.create (List.length parts) in
  List.iter
    (fun part ->
      if is_part_accepted workflows part then Hashtbl.add accepted part true
      else () )
    parts ;
  accepted |> Hashtbl.to_seq_keys
  |> Seq.fold_left (fun sum (x, m, a, s) -> sum + x + m + a + s) 0

let parse_input path =
  match Utils.file_to_string path |> Str.split (Str.regexp "\n\n") with
  | [workflows_str; parts_str] ->
      let workflows_lst = String.split_on_char '\n' workflows_str
      and parts_lst = String.split_on_char '\n' parts_str in
      let workflows = Hashtbl.create (List.length workflows_lst)
      and parts =
        List.map
          (fun part_str ->
            match
              String.split_on_char ','
                (String.sub part_str 1 (String.length part_str - 2))
            with
            | [_; _; _; _] as xmas -> (
              match
                xmas
                |> List.map (fun data ->
                       match String.split_on_char '=' data with
                       | [_; value] ->
                           int_of_string value
                       | _ ->
                           failwith ("Invalid part: " ^ part_str) )
              with
              | [x; m; a; s] ->
                  (x, m, a, s)
              | _ ->
                  failwith ("Invalid part: " ^ part_str) )
            | _ ->
                failwith ("Invalid part: " ^ part_str) )
          parts_lst
      in
      List.iter
        (fun workflow_str ->
          if Str.string_match (Str.regexp {|^\(.+\){\(.+\)}$|}) workflow_str 0
          then
            let workflow_name = Str.matched_group 1 workflow_str
            and workflow_rules_str = Str.matched_group 2 workflow_str in
            let workflow_rules =
              String.split_on_char ',' workflow_rules_str
              |> List.map (fun rule_str ->
                     if String.contains rule_str ':' then
                       match String.split_on_char ':' rule_str with
                       | [condition; result] -> (
                         match String.split_on_char '<' condition with
                         | [category; compare_num] ->
                             ( LessThan (category, int_of_string compare_num)
                             , map_to_workflow_result result )
                         | _ -> (
                           match String.split_on_char '>' condition with
                           | [category; compare_num] ->
                               ( GreaterThan
                                   (category, int_of_string compare_num)
                               , map_to_workflow_result result )
                           | _ ->
                               failwith ("Invalid condition: " ^ condition) ) )
                       | _ ->
                           failwith ("Invalid rule: " ^ rule_str)
                     else (AlwaysTrue, map_to_workflow_result rule_str) )
            in
            Hashtbl.add workflows workflow_name workflow_rules
          else failwith ("Invalid workflow: " ^ workflow_str) )
        workflows_lst ;
      (workflows, parts)
  | _ ->
      failwith "Invalid input."

let part_1_aux path =
  let workflows, parts = parse_input path in
  perform_workflows workflows parts

let test_1 () =
  part_1_aux "lib/day19/test.txt" |> print_int ;
  print_newline ()

let part_1 () = part_1_aux "lib/day19/input.txt"

let solution () =
  print_string "Part 1: " ;
  part_1 () |> print_int ;
  print_newline ()
(* ;
   print_string "Part 2: " ;
   part_2 () |> print_int ;
   print_newline () *)
