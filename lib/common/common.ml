let file_to_list path =
  let file = open_in path in
  let rec read_file acc =
    try input_line file :: acc |> read_file with End_of_file -> acc
  in
  read_file [] |> List.rev

let zip_with_index lst = List.mapi (fun idx el -> (idx, el)) lst

let fold_lefti fn acc lst =
  List.fold_left (fun acc (idx, el) -> fn acc idx el) acc (zip_with_index lst)

let get_neighbor_idxs (row_idx, col_idx) row_length col_length =
  let addends = List.init 3 (fun idx -> idx - 1)
  and is_valid_idx idx bound = idx >= 0 && idx < bound in
  addends
  |> List.map (fun row_addend ->
         addends
         |> List.filter_map (fun col_addend ->
                let row_coord = row_idx + row_addend
                and col_coord = col_idx + col_addend in
                if
                  (not (row_addend = 0 && col_addend = 0))
                  && is_valid_idx row_coord row_length
                  && is_valid_idx col_coord col_length
                then Some (row_coord, col_coord)
                else None ) )
  |> List.flatten
