let file_to_list path =
  let file = open_in path in
  let rec read_file acc =
    try input_line file :: acc |> read_file with End_of_file -> acc
  in
  read_file [] |> List.rev

let file_to_grid path =
  let file_lst = file_to_list path in
  let grid = Array.make (List.length file_lst) (Array.make 0 ' ') in
  List.iteri
    (fun idx line -> grid.(idx) <- line |> String.to_seq |> Array.of_seq)
    file_lst ;
  grid

let file_to_string path =
  let file = open_in_bin path in
  let content = really_input_string file (in_channel_length file) in
  close_in file ; content

let is_int num = float_of_int (int_of_float num) = num

let zip_with_index lst = List.mapi (fun idx el -> (idx, el)) lst

let fold_lefti fn acc lst =
  List.fold_left (fun acc (idx, el) -> fn acc idx el) acc (zip_with_index lst)

let rec map2_shortest fn lst1 lst2 =
  match (lst1, lst2) with
  | head1 :: tail1, head2 :: tail2 ->
      fn head1 head2 :: map2_shortest fn tail1 tail2
  | [], _ ->
      lst2
  | _, [] ->
      lst1

let is_valid_coord coord ?(lower_bound = 0) upper_bound =
  coord >= lower_bound && coord < upper_bound

let get_neighbor_idxs (row_idx, col_idx) row_length col_length =
  let addends = List.init 3 (fun idx -> idx - 1) in
  addends
  |> List.map (fun row_addend ->
         addends
         |> List.filter_map (fun col_addend ->
                let row_coord = row_idx + row_addend
                and col_coord = col_idx + col_addend in
                if
                  (not (row_addend = 0 && col_addend = 0))
                  && is_valid_coord row_coord row_length
                  && is_valid_coord col_coord col_length
                then Some (row_coord, col_coord)
                else None ) )
  |> List.flatten

let get_neighbor_idxs_cardinal_with_directions (row_idx, col_idx) row_length
    col_length =
  [ ((row_idx + 1, col_idx), Direction.Left)
  ; ((row_idx - 1, col_idx), Direction.Right)
  ; ((row_idx, col_idx + 1), Direction.Up)
  ; ((row_idx, col_idx - 1), Direction.Down) ]
  |> List.filter (fun ((x_coord, y_coord), _) ->
         is_valid_coord x_coord row_length && is_valid_coord y_coord col_length )

let get_neighbor_idxs_cardinal (row_idx, col_idx) row_length col_length =
  get_neighbor_idxs_cardinal_with_directions (row_idx, col_idx) row_length
    col_length
  |> List.map (fun (coords, _) -> coords)

let are_coords_valid x_coord y_coord grid =
  x_coord >= 0
  && x_coord < Array.length grid
  && y_coord >= 0
  && y_coord < Array.length grid.(0)

let find_max comparator lst =
  match
    List.fold_left
      (fun max value ->
        match max with
        | Some prevMax ->
            Some (if comparator prevMax value < 0 then value else prevMax)
        | None ->
            Some value )
      None lst
  with
  | Some max ->
      max
  | None ->
      failwith "List is empty."

let loop ?(step = 1) callback loop_from loop_to =
  if loop_from > loop_to then
    failwith
      ( string_of_int loop_to ^ " is greater than " ^ string_of_int loop_from
      ^ "." )
  else
    let rec loop_aux i =
      if i > loop_to then ()
      else (
        callback i ;
        loop_aux (i + step) )
    in
    loop_aux loop_from

let loop_with_break ?(step = 1) callback loop_from loop_to =
  if loop_from > loop_to then
    failwith
      ( string_of_int loop_to ^ " is greater than " ^ string_of_int loop_from
      ^ "." )
  else
    let rec loop_with_break_aux i =
      if i > loop_to then false
      else if callback i then true
      else loop_with_break_aux (i + step)
    in
    loop_with_break_aux loop_from

let transpose grid =
  let new_row_length = Array.length grid
  and new_col_length = Array.length grid.(0) in
  Array.init new_col_length (fun i ->
      Array.init new_row_length (fun j -> grid.(j).(i)) )

let clone_2d_grid grid =
  Array.init (Array.length grid) (fun idx -> Array.copy grid.(idx))

let reverse_array array =
  let length = Array.length array in
  Array.init length (fun idx -> array.(length - idx - 1))

let list_count_if predicate array =
  List.fold_left (fun count el -> count + if predicate el then 1 else 0) 0 array

let array_count_if predicate array =
  Array.fold_left
    (fun count el -> count + if predicate el then 1 else 0)
    0 array

let groupby_with_counts lst =
  match
    List.fold_left
      (fun (acc, prev, prev_count) el ->
        match prev with
        | None ->
            (acc, Some el, 1)
        | Some v ->
            if el = v then (acc, Some el, prev_count + 1)
            else ((v, prev_count) :: acc, Some el, 1) )
      ([], None, 0) lst
  with
  | _, None, _ ->
      []
  | acc, Some prev, prev_count ->
      List.rev ((prev, prev_count) :: acc)

let string_repeat n string =
  let rec string_repeat_aux n acc =
    if n = 1 then acc else string_repeat_aux (n - 1) (acc ^ string)
  in
  string_repeat_aux n string

let digit_char_to_number char =
  match char with
  | '0' .. '9' ->
      int_of_char char - int_of_char '0'
  | _ ->
      failwith (String.make 1 char ^ " is not a digit.")

let option_get opt =
  match opt with Some v -> v | None -> failwith "Option is None."

let rec get_at_index idx seq =
  if idx < 0 then failwith "Index cannot be negative."
  else
    match seq () with
    | Seq.Cons (head, tail) ->
        if idx = 0 then head else get_at_index (idx - 1) tail
    | Seq.Nil ->
        failwith "Index out of bounds."

let groupby grouper seq =
  Seq.fold_left
    (fun acc el ->
      (let group = grouper el in
       match Hashtbl.find_opt acc group with
       | Some group_lst ->
           Hashtbl.replace acc group (el :: group_lst)
       | None ->
           Hashtbl.replace acc group [el] ) ;
      acc )
    (Hashtbl.create (Seq.length seq / 2))
    seq
