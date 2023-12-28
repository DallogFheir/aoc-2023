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

let calculate_all_combinations workflows =
  let restrict_ranges category value is_first_in_tuple (x, m, a, s) =
    let restrict_range (range_start, range_end) =
      let new_range, opposite_range =
        if is_first_in_tuple then
          ( (max range_start (value + 1), range_end)
          , (range_start, min range_end value) )
        else
          ( (range_start, min range_end (value - 1))
          , (max range_start value, range_end) )
      in
      ( (if fst new_range > snd new_range then None else Some new_range)
      , if fst opposite_range > snd opposite_range then None
        else Some opposite_range )
    in
    let restrict_ranges_aux idx =
      let new_ranges = [|x; m; a; s|] and opposite_ranges = [|x; m; a; s|] in
      let new_range_opt, opposite_range_opt = restrict_range new_ranges.(idx) in
      let new_new_ranges =
        match new_range_opt with
        | Some new_range ->
            new_ranges.(idx) <- new_range ;
            Some new_ranges
        | _ ->
            None
      and new_opposite_ranges =
        match opposite_range_opt with
        | Some opposite_range ->
            opposite_ranges.(idx) <- opposite_range ;
            Some opposite_ranges
        | _ ->
            None
      in
      match (new_new_ranges, new_opposite_ranges) with
      | ( Some [|x_new; m_new; a_new; s_new|]
        , Some [|x_opposite; m_opposite; a_opposite; s_opposite|] ) ->
          ( Some (x_new, m_new, a_new, s_new)
          , Some (x_opposite, m_opposite, a_opposite, s_opposite) )
      | None, Some [|x_opposite; m_opposite; a_opposite; s_opposite|] ->
          (None, Some (x_opposite, m_opposite, a_opposite, s_opposite))
      | Some [|x_new; m_new; a_new; s_new|], None ->
          (Some (x_new, m_new, a_new, s_new), None)
      | _ ->
          failwith "How did this happen?"
    in
    match category with
    | "x" ->
        restrict_ranges_aux 0
    | "m" ->
        restrict_ranges_aux 1
    | "a" ->
        restrict_ranges_aux 2
    | "s" ->
        restrict_ranges_aux 3
    | _ ->
        failwith ("Invalid category: " ^ category)
  in
  let rec calculate_all_combinations_aux queue total =
    match queue with
    | [] ->
        total
    | ((rule, result) :: workflow_tail, ((x, m, a, s) as ranges)) :: tail -> (
        let restricted_ranges_opt, opposite_ranges_opt =
          match rule with
          | LessThan (category, value) ->
              restrict_ranges category value false ranges
          | GreaterThan (category, value) ->
              restrict_ranges category value true ranges
          | AlwaysTrue ->
              (Some ranges, None)
        in
        if restricted_ranges_opt = None then
          calculate_all_combinations_aux
            ((workflow_tail, Utils.option_get opposite_ranges_opt) :: tail)
            total
        else
          let restricted_ranges = Utils.option_get restricted_ranges_opt
          and next_queue =
            if opposite_ranges_opt = None then tail
            else (workflow_tail, Utils.option_get opposite_ranges_opt) :: tail
          in
          match result with
          | Accept ->
              let x, m, a, s = restricted_ranges in
              let to_add =
                [x; m; a; s]
                |> List.fold_left
                     (fun prod (range_start, range_end) ->
                       let range_length = range_end - range_start + 1 in
                       prod * range_length )
                     1
              in
              calculate_all_combinations_aux next_queue (total + to_add)
          | Reject ->
              calculate_all_combinations_aux next_queue total
          | SendTo other_workflow ->
              calculate_all_combinations_aux
                ( (Hashtbl.find workflows other_workflow, restricted_ranges)
                :: next_queue )
                total )
    | _ :: tail ->
        calculate_all_combinations_aux tail total
  in
  calculate_all_combinations_aux
    [(Hashtbl.find workflows "in", ((1, 4000), (1, 4000), (1, 4000), (1, 4000)))]
    0

let part_2_aux path =
  let workflows, _ = parse_input path in
  calculate_all_combinations workflows

let test_2 () =
  part_2_aux "lib/day19/test.txt" |> print_int ;
  print_newline ()

let part_2 () = part_2_aux "lib/day19/input.txt"

let solution () =
  print_endline "DAY 19" ;
  print_string "Part 1: " ;
  part_1 () |> print_int ;
  print_newline () ;
  print_string "Part 2: " ;
  part_2 () |> print_int ;
  print_newline ()
