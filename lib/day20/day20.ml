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

let press_button button_presses modules =
  let button_press_queue = [("broadcaster", "button", Low)] in
  let rec press_button_aux button_presses queue low_total high_total =
    if button_presses = 0 then low_total * high_total
    else
      match queue with
      | [] ->
          press_button_aux (button_presses - 1) button_press_queue low_total
            high_total
      | (receiver_name, sender_name, signal) :: tail ->
          let receiver = Hashtbl.find modules receiver_name in
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
                List.map (fun output -> (output, receiver_name, signal)) outputs
            | Untyped ->
                []
          in
          press_button_aux button_presses (tail @ new_queue) new_low_total
            new_high_total
  in
  press_button_aux button_presses button_press_queue 0 0

let part_1_aux path =
  Utils.file_to_list path |> parse_input |> press_button 1000

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
        if Hashtbl.mem already_seen head then get_subgraph_aux tail
        else (
          Hashtbl.add already_seen head true ;
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
        already_seen
  in
  Hashtbl.add already_seen "broadcaster" true ;
  get_subgraph_aux [end_module]

let test_1 () =
  part_1_aux "lib/day20/test1.txt" |> print_int ;
  print_newline () ;
  part_1_aux "lib/day20/test2.txt" |> print_int ;
  print_newline ()

let part_1 () = part_1_aux "lib/day20/input.txt"

let eda () = create_mermaid () |> print_string

let part_2 () =
  Utils.file_to_list "lib/day20/input.txt"
  |> parse_input |> get_subgraph "pr" |> Hashtbl.to_seq_keys |> List.of_seq
  |> List.sort (fun x y -> if x > y then 1 else -1)
  |> List.iter (fun modd -> print_endline modd) ;
  2

let solution () =
  print_string "Part 1: " ;
  part_1 () |> print_int ;
  print_newline () ;
  print_string "Part 2: " ;
  part_2 () |> print_int ;
  print_newline ()
