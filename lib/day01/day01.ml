let parse_int_from_char chr = int_of_char chr - int_of_char '0'

let convert_int_to_char i = char_of_int (i + int_of_char '0')

let find_first_and_last_digit str =
  let rec aux acc strLst =
    let first, _ = acc in
    match strLst with
    | [] ->
        acc
    | ('0' .. '9' as head) :: tail ->
        aux ((if first = ' ' then head else first), head) tail
    | _ :: tail ->
        aux acc tail
  in
  aux (' ', ' ') (str |> String.to_seq |> List.of_seq)

let find_word_digit str =
  let digit_words =
    [| ("one", 2)
     ; ("two", 2)
     ; ("three", 4)
     ; ("four", 4)
     ; ("five", 3)
     ; ("six", 3)
     ; ("seven", 4)
     ; ("eight", 4)
     ; ("nine", 3) |]
  in
  match
    digit_words
    |> Array.find_index (fun (word, _) ->
           String.length str >= String.length word
           && String.sub str 0 (String.length word) = word )
  with
  | Some idx ->
      Some (idx + 1, digit_words.(idx) |> snd)
  | None ->
      None

let find_first_and_last_digit_with_words str =
  let rec aux acc str =
    let first, _ = acc in
    match str with
    | "" ->
        acc
    | _ -> (
        let substr = String.sub str 1 (String.length str - 1) in
        match str.[0] with
        | '0' .. '9' as chr ->
            aux ((if first = ' ' then chr else first), chr) substr
        | _ -> (
          match find_word_digit str with
          | Some (digit, fg) ->
              let chr = convert_int_to_char digit in
              aux
                ((if first = ' ' then chr else first), chr)
                (String.sub str fg (String.length str - fg))
          | None ->
              aux acc substr ) )
  in
  aux (' ', ' ') str

let runner fn path =
  Utils.file_to_list path
  |> List.fold_left
       (fun acc el ->
         let first, last = fn el in
         (10 * parse_int_from_char first) + parse_int_from_char last + acc )
       0

let test_1 () =
  runner find_first_and_last_digit "lib/day01/test1.txt" |> print_int

let part_1 () = runner find_first_and_last_digit "lib/day01/input.txt"

let test_2 () =
  runner find_first_and_last_digit_with_words "lib/day01/test2.txt" |> print_int

let part_2 () =
  runner find_first_and_last_digit_with_words "lib/day01/input.txt"

let solution () =
  print_string "Part 1: " ;
  part_1 () |> print_int ;
  print_newline () ;
  print_string "Part 2: " ;
  part_2 () |> print_int ;
  print_newline ()
