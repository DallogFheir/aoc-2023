type hand =
  | HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind

let cards = [|'A'; 'K'; 'Q'; 'J'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2'|]

let cards_with_joker =
  [|'A'; 'K'; 'Q'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2'; 'J'|]

let get_hand_type hand =
  let counter = Hashtbl.create 5 in
  hand |> String.to_seq
  |> Seq.iter (fun card ->
         match Hashtbl.find_opt counter card with
         | Some count ->
             Hashtbl.replace counter card (count + 1)
         | None ->
             Hashtbl.add counter card 1 ) ;
  let counts =
    Hashtbl.to_seq_values counter
    |> List.of_seq
    |> List.stable_sort (fun a b -> a - b)
  and counts_length = Hashtbl.length counter in
  match (counts_length, counts) with
  | 1, _ ->
      FiveOfAKind
  | 2, [1; 4] ->
      FourOfAKind
  | 2, _ ->
      FullHouse
  | 3, [1; 1; 3] ->
      ThreeOfAKind
  | 3, _ ->
      TwoPair
  | 4, _ ->
      OnePair
  | 5, _ ->
      HighCard
  | _ ->
      failwith ("Invalid card count: " ^ string_of_int counts_length)

let get_hand_type_with_joker hand =
  let counter = Hashtbl.create 5 in
  let joker_count =
    hand |> String.to_seq
    |> Seq.fold_left
         (fun count card ->
           ( match Hashtbl.find_opt counter card with
           | Some count ->
               Hashtbl.replace counter card (count + 1)
           | None ->
               Hashtbl.add counter card 1 ) ;
           count + if card = 'J' then 1 else 0 )
         0
  and counts =
    Hashtbl.to_seq_values counter
    |> List.of_seq
    |> List.stable_sort (fun a b -> a - b)
  and counts_length = Hashtbl.length counter in
  match (counts_length, counts, joker_count) with
  | 1, _, _ ->
      FiveOfAKind
  | 2, [1; 4], 0 ->
      FourOfAKind
  | 2, _, 0 ->
      FullHouse
  | 2, _, _ ->
      FiveOfAKind
  | 3, [1; 1; 3], 0 ->
      ThreeOfAKind
  | 3, _, 0 ->
      TwoPair
  | 3, [1; 1; 3], _ ->
      FourOfAKind
  | 3, _, 1 ->
      FullHouse
  | 3, _, 2 ->
      FourOfAKind
  | 4, _, 0 ->
      OnePair
  | 4, _, _ ->
      ThreeOfAKind
  | 5, _, 0 ->
      HighCard
  | 5, _, _ ->
      OnePair
  | _ ->
      failwith ("Invalid card count: " ^ string_of_int counts_length)

let compare_hands hand_type_getter cards_order hand_1 hand_2 =
  let hand_1_type = hand_type_getter hand_1
  and hand_2_type = hand_type_getter hand_2 in
  if hand_1_type > hand_2_type then 1
  else if hand_1_type < hand_2_type then -1
  else
    let rec compare_same_type hand_1_seq hand_2_seq =
      match (hand_1_seq, hand_2_seq) with
      | [], [] ->
          0
      | head_1 :: tail_1, head_2 :: tail_2 -> (
          if head_1 = head_2 then compare_same_type tail_1 tail_2
          else
            match
              ( Array.find_index (fun x -> x = head_1) cards_order
              , Array.find_index (fun x -> x = head_2) cards_order )
            with
            | Some idx_1, Some idx_2 ->
                idx_2 - idx_1
            | None, _ ->
                failwith ("Invalid card: " ^ String.make 1 head_1)
            | _, None ->
                failwith ("Invalid card: " ^ String.make 1 head_2) )
      | _, _ ->
          failwith "Hands have different lengths."
    in
    compare_same_type
      (String.to_seq hand_1 |> List.of_seq)
      (String.to_seq hand_2 |> List.of_seq)

let calculate_winnings hand_type_getter cards_order path =
  Utils.file_to_list path
  |> List.map (fun line ->
         match String.split_on_char ' ' line with
         | [hand; bid] ->
             (hand, int_of_string bid)
         | _ ->
             failwith "Invalid input line." )
  |> List.stable_sort (fun (hand_1, _) (hand_2, _) ->
         compare_hands hand_type_getter cards_order hand_1 hand_2 )
  |> Utils.fold_lefti
       (fun sum idx (_, bid) ->
         let rank = idx + 1 in
         sum + (rank * bid) )
       0

let part_1_aux path = calculate_winnings get_hand_type cards path

let part_2_aux path =
  calculate_winnings get_hand_type_with_joker cards_with_joker path

let test_1 () =
  part_1_aux "lib/day07/test.txt" |> print_int ;
  print_newline ()

let part_1 () = part_1_aux "lib/day07/input.txt"

let test_2 () =
  part_2_aux "lib/day07/test.txt" |> print_int ;
  print_newline ()

let part_2 () = part_2_aux "lib/day07/input.txt"

let solution () =
  print_string "Part 1: " ;
  part_1 () |> print_int ;
  print_newline () ;
  print_string "Part 2: " ;
  part_2 () |> print_int ;
  print_newline ()
