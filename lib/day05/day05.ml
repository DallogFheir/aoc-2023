open Rangedict

let input_part_to_map input_without_seeds prefix =
  List.find (fun part -> String.starts_with ~prefix part) input_without_seeds
  |> String.split_on_char '\n' |> List.tl
  |> List.fold_left
       (fun rd line ->
         match String.split_on_char ' ' line with
         | [dest_start_str; source_start_str; range_length_str] ->
             let dest_start = int_of_string dest_start_str
             and source_start = int_of_string source_start_str
             and range_length = int_of_string range_length_str in
             let source_range =
               RangeDict.create_range source_start
                 (source_start + range_length - 1)
             in
             RangeDict.add source_range dest_start rd
         | _ ->
             failwith "Line does not contain 3 spaces." )
       RangeDict.empty

let part_1_aux path =
  let input_parts =
    Utils.file_to_string path |> Str.split (Str.regexp "\n\n")
  in
  match input_parts with
  | seeds_line :: rest ->
      let seeds_pattern = Str.regexp {|seeds: \(.+\)|} in
      let seeds_values =
        if Str.string_match seeds_pattern seeds_line 0 then
          Str.matched_group 1 seeds_line
          |> String.split_on_char ' '
          |> List.map (fun value -> int_of_string value)
        else failwith "Pattern did not match seeds line."
      and maps =
        [ "seed-to-soil"
        ; "soil-to-fertilizer"
        ; "fertilizer-to-water"
        ; "water-to-light"
        ; "light-to-temperature"
        ; "temperature-to-humidity"
        ; "humidity-to-location" ]
        |> List.map (fun prefix -> input_part_to_map rest prefix)
      in
      seeds_values
      |> List.map (fun seed_value ->
             List.fold_left
               (fun acc map ->
                 match RangeDict.get_with_range acc map with
                 | Some ((range_start, _), value) ->
                     let offset = acc - range_start in
                     value + offset
                 | None ->
                     acc )
               seed_value maps )
      |> Utils.find_max (fun a b -> b - a)
  | _ ->
      failwith "Input parts are empty."

let rec get_seed_ranges acc lst =
  match lst with
  | range_start :: range_length :: tail ->
      let seed_range =
        RangeDict.create_range range_start (range_start + range_length)
      in
      get_seed_ranges (seed_range :: acc) tail
  | [] ->
      List.rev acc
  | _ ->
      failwith "Invalid seeds values."

let find_next_ranges range_lst map =
  let rec parse_overlapping acc
      ((remaining_range_start, remaining_range_end) as range) lst =
    match lst with
    | ((range_start, range_end), value) :: tail ->
        if remaining_range_start < range_start then
          let leftover =
            RangeDict.create_range remaining_range_start (range_start - 1)
          in
          parse_overlapping (leftover :: acc)
            (range_start, remaining_range_end)
            lst
        else if remaining_range_start > range_start then
          let newOverlapping =
            ( RangeDict.create_range remaining_range_start range_end
            , value + (remaining_range_start - range_start) )
          in
          parse_overlapping acc range (newOverlapping :: tail)
        else if remaining_range_end <= range_end then
          RangeDict.create_range value
            (value + (remaining_range_end - remaining_range_start))
          :: acc
        else
          let leftover =
            RangeDict.create_range (range_end + 1) remaining_range_end
          and overlapping =
            RangeDict.create_range value (value + (range_end - range_start))
          in
          parse_overlapping (overlapping :: acc) leftover tail
    | [] ->
        range :: acc
  in
  List.map
    (fun range ->
      let overlapping = RangeDict.find_all_overlapping range map in
      parse_overlapping [] range overlapping )
    range_lst
  |> List.flatten

let part_2_aux path =
  let input_parts =
    Utils.file_to_string path |> Str.split (Str.regexp "\n\n")
  in
  match input_parts with
  | seeds_line :: rest ->
      let seeds_pattern = Str.regexp {|seeds: \(.+\)|} in
      let seeds_values =
        if Str.string_match seeds_pattern seeds_line 0 then
          Str.matched_group 1 seeds_line
          |> String.split_on_char ' '
          |> List.map (fun value -> int_of_string value)
        else failwith "Pattern did not match seeds line."
      and maps =
        [ "seed-to-soil"
        ; "soil-to-fertilizer"
        ; "fertilizer-to-water"
        ; "water-to-light"
        ; "light-to-temperature"
        ; "temperature-to-humidity"
        ; "humidity-to-location" ]
        |> List.map (fun prefix -> input_part_to_map rest prefix)
      in
      seeds_values |> get_seed_ranges []
      |> List.map (fun seed_range ->
             List.fold_left
               (fun acc map -> find_next_ranges acc map)
               [seed_range] maps
             |> List.map (fun (range_start, _) -> range_start)
             |> Utils.find_max (fun a b -> b - a) )
      |> Utils.find_max (fun a b -> b - a)
  | _ ->
      failwith "Input parts are empty."

let test_1 () = part_1_aux "lib/day05/test.txt" |> print_int

let part_1 () = part_1_aux "lib/day05/input.txt"

let test_2 () = part_2_aux "lib/day05/test.txt" |> print_int

let part_2 () = part_2_aux "lib/day05/input.txt"

let solution () =
  print_string "Part 1: " ;
  part_1 () |> print_int ;
  print_newline () ;
  print_string "Part 2: " ;
  part_2 () |> print_int ;
  print_newline ()
