let add_default_to_graph graph key value =
  match Hashtbl.find_opt graph key with
  | Some neighbors ->
      Hashtbl.replace neighbors value 1
  | None ->
      let neighbors = Hashtbl.create 50 in
      Hashtbl.replace neighbors value 1 ;
      Hashtbl.replace graph key neighbors

let create_graph path =
  let graph = Hashtbl.create 1000 in
  Utils.file_to_list path
  |> List.iter (fun line ->
         match Str.split (Str.regexp ": ") line with
         | source :: dests :: _ ->
             String.split_on_char ' ' dests
             |> List.iter (fun dest ->
                    add_default_to_graph graph dest source ;
                    add_default_to_graph graph source dest )
         | _ ->
             failwith ("Cannot split " ^ line ^ " on : .") ) ;
  graph

let merge_node_names node_1 node_2 =
  let lst_1 = String.split_on_char ',' node_1
  and lst_2 = String.split_on_char ',' node_2 in
  let res =
    List.sort (fun x y -> if x > y then -1 else 1) (lst_1 @ lst_2)
    |> List.fold_left (fun acc el -> el ^ "," ^ acc) ""
  in
  String.sub res 0 (String.length res - 1)

let get_random_node hashtbl =
  let node_idx = Random.int (Hashtbl.length hashtbl) in
  Hashtbl.to_seq_keys hashtbl |> Utils.get_at_index node_idx

let merge_neighbors random_node neighbors random_neighbor neighbors_neighbors =
  let new_neighbors =
    Hashtbl.create
      (Hashtbl.length neighbors + Hashtbl.length neighbors_neighbors)
  in
  Hashtbl.iter
    (fun neighbor_name neighbor_count ->
      if neighbor_name <> random_neighbor then
        match Hashtbl.find_opt neighbors_neighbors neighbor_name with
        | Some count ->
            Hashtbl.replace new_neighbors neighbor_name (neighbor_count + count) ;
            Hashtbl.remove neighbors_neighbors neighbor_name
        | None ->
            Hashtbl.replace new_neighbors neighbor_name neighbor_count )
    neighbors ;
  Hashtbl.iter
    (fun neighbor_name neighbor_count ->
      if neighbor_name <> random_node then
        Hashtbl.replace new_neighbors neighbor_name neighbor_count )
    neighbors_neighbors ;
  new_neighbors

let replace_merge_node_in_neighbor graph random_node random_neighbor
    merged_node_name neighbor_name _ =
  let neighbors_neighbors = Hashtbl.find graph neighbor_name in
  let new_count =
    ( match Hashtbl.find_opt neighbors_neighbors random_node with
    | Some count ->
        count
    | None ->
        0 )
    +
    match Hashtbl.find_opt neighbors_neighbors random_neighbor with
    | Some count ->
        count
    | None ->
        0
  in
  Hashtbl.remove neighbors_neighbors random_node ;
  Hashtbl.remove neighbors_neighbors random_neighbor ;
  Hashtbl.replace neighbors_neighbors merged_node_name new_count

let rec karger graph =
  if Hashtbl.length graph = 2 then graph
  else
    let random_node = get_random_node graph in
    let neighbors = Hashtbl.find graph random_node in
    let random_neighbor, _ =
      Utils.get_at_index
        (Random.int (Hashtbl.length neighbors))
        (Hashtbl.to_seq neighbors)
    in
    let merged_node = merge_node_names random_node random_neighbor
    and neighbors_neighbors = Hashtbl.find graph random_neighbor in
    let merged_neighbors =
      merge_neighbors random_node neighbors random_neighbor neighbors_neighbors
    in
    Hashtbl.remove graph random_node ;
    Hashtbl.remove graph random_neighbor ;
    Hashtbl.replace graph merged_node merged_neighbors ;
    Hashtbl.iter
      (replace_merge_node_in_neighbor graph random_node random_neighbor
         merged_node )
      merged_neighbors ;
    karger graph

let extract_components graph =
  match Hashtbl.to_seq_values graph |> List.of_seq with
  | [first_values; second_values] -> (
    match
      ( Hashtbl.to_seq first_values |> List.of_seq
      , Hashtbl.to_seq second_values |> List.of_seq )
    with
    | [(first, count)], [(second, _)] ->
        (first, second, count)
    | _ ->
        failwith "Some component has more than 1 neighbor." )
  | _ ->
      failwith "Graph is not of size 2."

let multiply_component_sizes c1 c2 =
  (String.split_on_char ',' c1 |> List.length)
  * (String.split_on_char ',' c2 |> List.length)

let rec part_1_aux path =
  let first, second, count =
    create_graph path |> karger |> extract_components
  in
  if count <> 3 then part_1_aux path else multiply_component_sizes first second

let test_1 () = part_1_aux "lib/day25/test.txt"

let part_1 () = part_1_aux "lib/day25/input.txt"

let solution () =
  print_endline "DAY 25" ;
  print_string "Part 1: " ;
  part_1 () |> print_int ;
  print_newline ()
