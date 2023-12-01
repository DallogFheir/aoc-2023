let file_to_list path =
  let file = open_in path in
  let rec read_file acc =
    try input_line file :: acc |> read_file with End_of_file -> acc
  in
  read_file [] |> List.rev
