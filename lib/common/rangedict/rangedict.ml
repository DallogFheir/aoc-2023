module Range = struct
  type t = int * int

  let overlaps (start_a, end_a) (start_b, end_b) =
    (start_a <= end_b && start_b <= end_a)
    || (start_b <= end_a && start_a <= end_b)

  let compare range1 range2 =
    if overlaps range1 range2 then 0 else fst range1 - fst range2
end

module RangeDictMap = Map.Make (Range)

module type RANGE_DICT = sig
  type range = int * int

  exception Invalid_range

  exception Overlapping_range

  val empty : 'a RangeDictMap.t

  val create_range : int -> int -> range

  val add :
    range -> 'a -> ('a * range) RangeDictMap.t -> ('a * range) RangeDictMap.t

  val get_with_range : int -> ('a * range) RangeDictMap.t -> (range * 'a) option

  val get : int -> ('a * range) RangeDictMap.t -> 'a option

  val find_all_overlapping :
    range -> ('a * range) RangeDictMap.t -> (range * 'a) list
end

module RangeDict : RANGE_DICT = struct
  type range = Range.t

  exception Invalid_range

  exception Overlapping_range

  let empty = RangeDictMap.empty

  let create_range range_start range_end =
    if range_end < range_start then raise Invalid_range
    else (range_start, range_end)

  let add range value rd =
    match RangeDictMap.find_opt range rd with
    | Some _ ->
        raise Overlapping_range
    | None ->
        RangeDictMap.add range (value, range) rd

  let get_with_range num rd =
    match RangeDictMap.find_opt (create_range num num) rd with
    | Some (value, range) ->
        Some (range, value)
    | None ->
        None

  let get num rd =
    match get_with_range num rd with
    | Some (_, value) ->
        Some value
    | None ->
        None

  let find_all_overlapping range rd =
    let rec find_all_overlapping_aux acc lst =
      match lst with
      | (head_range, (value, _)) :: tail ->
          if Range.overlaps head_range range then
            find_all_overlapping_aux ((head_range, value) :: acc) tail
          else find_all_overlapping_aux acc tail
      | [] ->
          List.rev acc
    in
    find_all_overlapping_aux [] (RangeDictMap.to_list rd)
end
