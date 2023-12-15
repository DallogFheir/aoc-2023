let hash str =
  str |> String.to_seq
  |> Seq.fold_left (fun sum char -> (Char.code char + sum) * 17 mod 256) 0

let part_1_aux path =
  Utils.file_to_string path |> String.split_on_char ','
  |> List.fold_left (fun sum step -> sum + hash step) 0

let test_1 () =
  part_1_aux "lib/day15/test.txt" |> print_int ;
  print_newline ()

let part_1 () = part_1_aux "lib/day15/input.txt"

let part_2_aux path =
  let rec replace_or_add_lens lens box =
    match box with
    | [] ->
        [lens]
    | head_lens :: tail ->
        if fst head_lens = fst lens then lens :: tail
        else head_lens :: replace_or_add_lens lens tail
  in
  let boxes = Array.init 256 (fun _ -> []) in
  Utils.file_to_string path |> String.split_on_char ','
  |> List.iter (fun step ->
         if String.contains step '=' then
           match String.split_on_char '=' step with
           | [label; focal_length] ->
               let box_num = hash label in
               let old_lenses = boxes.(box_num) in
               boxes.(box_num) <-
                 replace_or_add_lens
                   (label, int_of_string focal_length)
                   old_lenses
           | _ ->
               failwith ("Invalid step: " ^ step)
         else if String.ends_with ~suffix:"-" step then
           let label = String.sub step 0 (String.length step - 1) in
           let box_num = hash label in
           let old_lenses = boxes.(box_num) in
           boxes.(box_num) <-
             List.filter (fun (old_label, _) -> old_label <> label) old_lenses
         else failwith ("Invalid step: " ^ step) ) ;
  boxes
  |> Array.fold_left
       (fun (box_num, sum) box ->
         ( box_num + 1
         , sum
           + ( List.fold_left
                 (fun (lens_slot, sum) (_, focal_length) ->
                   ( lens_slot + 1
                   , sum + ((1 + box_num) * lens_slot * focal_length) ) )
                 (1, 0) box
             |> snd ) ) )
       (0, 0)
  |> snd

let test_2 () =
  part_2_aux "lib/day15/test.txt" |> print_int ;
  print_newline ()

let part_2 () = part_2_aux "lib/day15/input.txt"

let solution () =
  print_string "Part 1: " ;
  part_1 () |> print_int ;
  print_newline () ;
  print_string "Part 2: " ;
  part_2 () |> print_int ;
  print_newline ()
