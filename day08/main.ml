open Utils.Extensions

let digits_segments =
  [
    (0, [ 'a'; 'b'; 'c'; 'e'; 'f'; 'g' ]);
    (1, [ 'c'; 'f' ]);
    (2, [ 'a'; 'c'; 'd'; 'e'; 'g' ]);
    (3, [ 'a'; 'c'; 'd'; 'f'; 'g' ]);
    (4, [ 'b'; 'c'; 'd'; 'f' ]);
    (5, [ 'a'; 'b'; 'd'; 'f'; 'g' ]);
    (6, [ 'a'; 'b'; 'd'; 'e'; 'f'; 'g' ]);
    (7, [ 'a'; 'c'; 'f' ]);
    (8, [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g' ]);
    (9, [ 'a'; 'b'; 'c'; 'd'; 'f'; 'g' ]);
  ]

let string_to_char_list p = List.map (fun s -> s.[0]) (Str.split_to_char p)

let to_patterns s =
  let i_o = Str.split (Str.regexp " | ") s in
  match i_o with
  | [ input; output ] ->
      (String.split_on_char ' ' input, String.split_on_char ' ' output)
  | _ -> failwith "Bad input"

let count_1_4_7_8 l =
  List.fold_left
    (fun acc a ->
      match String.length a with 2 | 4 | 3 | 7 -> acc + 1 | _ -> acc)
    0 l

let matching_nums p =
  let s = List.map (fun (d, s) -> (d, List.length s)) digits_segments in
  List.assoc_value_all (String.length p) s

let build_combination (p, nums) = 
  List.fold_left (fun acc num -> 
    let num_segments = List.assoc num digits_segments in
    let p_segments = string_to_char_list p in
    acc @ List.flat_map (fun ns -> [(ns, p_segments)]) num_segments
  ) [] nums

let rec iterate_match (c, segments) choices possibles = 
  let segments = List.filter (fun s -> not(List.exists(fun (_,b) -> s = b) choices)) segments in
  List.fold_left (fun acc s -> 
    let choices = choices @ [(c, s)] in
    let possibles = List.filter (fun (pc, _) -> pc != c) possibles in
    match possibles with
    | [] -> [choices]
    | _ -> acc @ iterate_match (List.hd possibles) choices (List.tl possibles)
  ) [] segments

let get_digit p choices = 
  let segments = string_to_char_list p in
  let digit_segments = List.sort compare (List.map (fun s -> match List.find_opt (fun (_, o) -> o = s) choices with | None -> 'z' | Some (a,_) -> a) segments) in
  let digit_segment = List.find_opt (fun (_, s) -> List.equal (fun a b -> a = b) digit_segments s ) digits_segments in
  match digit_segment with 
    | None -> -1
    | Some (digit, _) -> digit

let check_combination patterns choices = 
  List.fold_left (fun acc p -> 
    match acc with
      | None -> acc
      | Some digits -> 
        let digit = get_digit p choices in
        if digit = -1 then None
        else Some (digits @ [digit])
  ) (Some []) patterns

let sort_digit_matches (p1, nums1) (p2, nums2) = compare (List.length nums1 * String.length p1) (List.length nums2 * String.length p2)

let keep_possible_combinations combinations = iterate_match (List.hd combinations) [] (List.tl combinations)

let digits_of_combinations_matching patterns = List.fold_left (fun acc c -> let result = (check_combination patterns c) in match result with | None -> acc | Some r -> r ) []

let build_score digits = List.sub 10 13 digits |> fun l -> (Int.mul (List.nth l 0) 1000) + (Int.mul (List.nth l 1) 100) + (Int.mul (List.nth l 2) 10) + (List.nth l 3)

let run_p1 l = List.map (fun a -> to_patterns a) l |> List.fold_left (fun acc (_, output) -> acc + count_1_4_7_8 output) 0

let run_p2 l =
  let patterns = List.map (fun a -> to_patterns a) l in
  List.fold_left(fun acc (i, o) -> acc + (
  let line_patterns = i @ o in line_patterns
  |> List.map (fun p -> (p, matching_nums p))
  |> List.sort sort_digit_matches 
  |> List.flat_map build_combination
  |> keep_possible_combinations
  |> digits_of_combinations_matching line_patterns
  |> build_score)
  ) 0 patterns

let run input =
  let t1 = Sys.time () in
  let part_1 = run_p1 input in
  let d_1 = Sys.time () -. t1 in
  let t2 = Sys.time () in
  let part_2 = run_p2 input in
  let d_2 = Sys.time () -. t2 in
  Printf.printf "Day 08 - Part 1: %d in %fs\n" part_1 d_1;
  Printf.printf "Day 08 - Part 2: %d in %fs\n" part_2 d_2
