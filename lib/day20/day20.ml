type pulse = Low | High

type communication_module =
  | FlipFlop of bool * string list
  | Conjunction of (string, pulse) Hashtbl.t * string list
  | Broadcast of string list
  | Untyped

let get_module_name module_info =
  if module_info.[0] = '%' || module_info.[0] = '&' then
    String.sub module_info 1 (String.length module_info - 1)
  else module_info

let parse_input lines =
  let module_to_outputs = Hashtbl.create (List.length lines)
  and module_to_inputs = Hashtbl.create (List.length lines)
  and parsed_modules = Hashtbl.create (List.length lines) in
  lines
  |> List.map (fun line ->
         match Str.split (Str.regexp {| -> |}) line with
         | [module_info; outputs_str] ->
             let outputs = Str.split (Str.regexp {|, |}) outputs_str
             and module_name = get_module_name module_info in
             Hashtbl.replace module_to_outputs module_name outputs ;
             List.iter
               (fun output ->
                 match Hashtbl.find_opt module_to_inputs output with
                 | Some inputs ->
                     Hashtbl.replace module_to_inputs output
                       (module_name :: inputs)
                 | None ->
                     Hashtbl.replace module_to_inputs output [module_name] )
               outputs ;
             module_info
         | _ ->
             failwith ("Invalid input: " ^ line) )
  |> List.iter (fun module_info ->
         let module_name = get_module_name module_info in
         let outputs = Hashtbl.find module_to_outputs module_name in
         Hashtbl.replace parsed_modules module_name
           ( if module_info.[0] = '%' then FlipFlop (false, outputs)
             else if module_info.[0] = '&' then (
               let inputs = Hashtbl.find module_to_inputs module_name in
               let inputs_hashtbl = Hashtbl.create (List.length inputs) in
               List.iter
                 (fun input -> Hashtbl.replace inputs_hashtbl input Low)
                 inputs ;
               Conjunction (inputs_hashtbl, outputs) )
             else if module_name = "broadcaster" then Broadcast outputs
             else Untyped ) ;
         List.iter
           (fun output ->
             if not (Hashtbl.mem parsed_modules output) then
               Hashtbl.replace parsed_modules output Untyped )
           outputs ) ;
  parsed_modules

let press_button ?(do_save_queue_idx = fun modules -> false) button_presses
    modules =
  let button_press_queue = [("broadcaster", "button", Low)] in
  let rec press_button_aux button_presses queue low_total high_total queue_idx
      saved_queue_idxs =
    if button_presses = 0 then (low_total, high_total, modules, saved_queue_idxs)
    else
      match queue with
      | [] ->
          press_button_aux (button_presses - 1) button_press_queue low_total
            high_total 0 saved_queue_idxs
      | (receiver_name, sender_name, signal) :: tail -> (
          let new_saved_queue_idxs =
            if do_save_queue_idx modules then queue_idx :: saved_queue_idxs
            else saved_queue_idxs
          in
          let receiver_opt = Hashtbl.find_opt modules receiver_name in
          match receiver_opt with
          | None ->
              press_button_aux button_presses tail low_total high_total
                (queue_idx + 1) new_saved_queue_idxs
          | Some receiver ->
              let new_low_total = low_total + if signal = Low then 1 else 0
              and new_high_total = high_total + if signal = High then 1 else 0
              and new_queue =
                match receiver with
                | FlipFlop (on, outputs) ->
                    if signal = High then []
                    else (
                      Hashtbl.replace modules receiver_name
                        (FlipFlop (not on, outputs)) ;
                      List.map
                        (fun output ->
                          (output, receiver_name, if on then Low else High) )
                        outputs )
                | Conjunction (inputs, outputs) ->
                    Hashtbl.replace inputs sender_name signal ;
                    let signal_to_send =
                      if
                        Hashtbl.to_seq_values inputs
                        |> Seq.for_all (fun input_signal -> input_signal = High)
                      then Low
                      else High
                    in
                    List.map
                      (fun output -> (output, receiver_name, signal_to_send))
                      outputs
                | Broadcast outputs ->
                    List.map
                      (fun output -> (output, receiver_name, signal))
                      outputs
                | Untyped ->
                    []
              in
              press_button_aux button_presses (tail @ new_queue) new_low_total
                new_high_total (queue_idx + 1) new_saved_queue_idxs )
  in
  press_button_aux button_presses button_press_queue 0 0 0 []

let part_1_aux path =
  let low_total, high_total, _, _ =
    Utils.file_to_list path |> parse_input |> press_button 1000
  in
  low_total * high_total

let create_mermaid () =
  Utils.file_to_list "lib/day20/input.txt"
  |> List.fold_left
       (fun acc line ->
         match Str.split (Str.regexp {| -> |}) line with
         | [module_info; outputs_str] ->
             let module_name = get_module_name module_info in
             let module_suffix =
               if module_info.[0] = '%' then "((" ^ module_name ^ "))"
               else if module_info.[0] = '&' then ""
               else if module_info = "broadcaster" then "{" ^ module_name ^ "}"
               else failwith ("Invalid module " ^ module_info)
             in
             let module_node = module_name ^ module_suffix
             and outputs = Str.split (Str.regexp {|, |}) outputs_str in
             outputs
             |> List.fold_left
                  (fun acc output ->
                    acc ^ "  " ^ module_node ^ " --> " ^ output ^ "\n" )
                  acc
         | _ ->
             failwith ("Invalid input: " ^ line) )
       "flowchart TD\n"

let get_subgraph end_module modules =
  let modules_seq = Hashtbl.to_seq modules
  and already_seen = Hashtbl.create 20 in
  let rec get_subgraph_aux queue =
    match queue with
    | head :: tail ->
        if Hashtbl.mem already_seen head || head = "broadcaster" then
          get_subgraph_aux tail
        else (
          Hashtbl.add already_seen head (Hashtbl.find modules head) ;
          modules_seq
          |> Seq.fold_left
               (fun acc (module_name, module_info) ->
                 match module_info with
                 | FlipFlop (_, outputs) | Conjunction (_, outputs) ->
                     if List.mem head outputs then acc @ (module_name :: outputs)
                     else acc
                 | _ ->
                     acc )
               tail
          |> get_subgraph_aux )
    | _ ->
        ( match Hashtbl.find modules "broadcaster" with
        | Broadcast outputs ->
            let new_outputs =
              List.filter
                (fun output -> Hashtbl.mem already_seen output)
                outputs
            in
            Hashtbl.replace already_seen "broadcaster" (Broadcast new_outputs)
        | _ ->
            failwith "Broadcaster is not a broadcaster." ) ;
        already_seen
  in
  get_subgraph_aux [end_module]

let find_low_pulses target graph =
  let rec find_low_pulses_aux res history idx =
    match history with
    | last_state :: _ -> (
        let _, _, new_state, correct_queue_idxs =
          press_button 1 (Hashtbl.copy last_state)
            ~do_save_queue_idx:(fun modules ->
              match Hashtbl.find modules target with
              | Conjunction (inputs, _) ->
                  Hashtbl.to_seq_values inputs
                  |> Seq.for_all (fun pulse -> pulse = High)
              | _ ->
                  failwith "Target should be a conjunction module." )
        in
        let new_res =
          List.map (fun queue_idx -> (idx + 1, queue_idx)) correct_queue_idxs
          @ res
        in
        let already_idx_opt =
          List.find_index (fun state -> state = new_state) history
        in
        match already_idx_opt with
        | Some already_idx ->
            (new_res, already_idx + 1)
        | None ->
            find_low_pulses_aux new_res (new_state :: history) (idx + 1) )
    | _ ->
        failwith "Empty history."
  in
  find_low_pulses_aux [] [graph] 0

let test_1 () =
  part_1_aux "lib/day20/test1.txt" |> print_int ;
  print_newline () ;
  part_1_aux "lib/day20/test2.txt" |> print_int ;
  print_newline ()

let part_1 () = part_1_aux "lib/day20/input.txt"

let eda () = create_mermaid () |> print_string

let prime_factorize num =
  let rec prime_factorize_aux num factor factors =
    if num = 1 then factors
    else if num mod factor = 0 then (
      ( match Hashtbl.find_opt factors factor with
      | Some count ->
          Hashtbl.replace factors factor (count + 1)
      | None ->
          Hashtbl.replace factors factor 1 ) ;
      prime_factorize_aux (num / factor) factor factors )
    else prime_factorize_aux num (factor + 1) factors
  in
  prime_factorize_aux num 2 (Hashtbl.create 10)

let find_lcm num_lst =
  let lcm_factors = Hashtbl.create (List.length num_lst) in
  List.iter
    (fun num ->
      let factors = prime_factorize num in
      Hashtbl.to_seq factors
      |> Seq.iter (fun (factor, count) ->
             match Hashtbl.find_opt lcm_factors factor with
             | Some lcm_count ->
                 Hashtbl.replace lcm_factors factor
                   (if count > lcm_count then count else lcm_count)
             | None ->
                 Hashtbl.replace lcm_factors factor count ) )
    num_lst ;
  Hashtbl.to_seq lcm_factors
  |> Seq.fold_left
       (fun acc (factor, count) ->
         acc * int_of_float (float_of_int factor ** float_of_int count) )
       1

let part_2 () =
  let graph = Utils.file_to_list "lib/day20/input.txt" |> parse_input
  and nodes = ["jv"; "qs"; "jm"; "pr"] in
  let res =
    nodes
    |> List.map (fun node ->
           graph |> get_subgraph node |> find_low_pulses node )
  in
  let queue_idxs = Hashtbl.create 10 in
  res
  |> List.iter (fun (all_high_pulses_idxs, _) ->
         List.iter
           (fun (press_idx, queue_idx) ->
             match Hashtbl.find_opt queue_idxs queue_idx with
             | Some press_idxs ->
                 Hashtbl.replace queue_idxs queue_idx (press_idx :: press_idxs)
             | None ->
                 Hashtbl.replace queue_idxs queue_idx [press_idx] )
           all_high_pulses_idxs ) ;
  match
    Hashtbl.to_seq_values queue_idxs
    |> Seq.fold_left
         (fun acc cycle_lengths ->
           if List.length cycle_lengths <> List.length nodes then acc
           else
             let lcm = find_lcm cycle_lengths in
             match acc with
             | Some min ->
                 if lcm < min then Some lcm else Some min
             | None ->
                 Some lcm )
         None
  with
  | Some min ->
      print_int min
  | None ->
      failwith "Did not find minimum."

let solution () =
  print_string "Part 1: " ;
  part_1 () |> print_int ;
  print_newline () ;
  print_string "Part 2: " ;
  part_2 () ;
  print_newline ()
