open Utils.Extensions

type pair_info = { polymers: string list; produce : string * string; count: int; increment: string}

let to_pair p = match Str.split (Str.regexp " -> ") p with 
  | i::[o] -> (let e = Str.split_to_char i in match e with | a::[b] -> (i, {polymers=[a;b];produce=(a ^ o,o ^ b);count=0;increment=o}) | _ -> failwith "Bad input")
  | _ -> failwith "Bad input"

let parse_input l = match l with template::_::pairs -> (template, List.map to_pair pairs) | _ -> failwith "Bad input"

let update_pair_counts p count pairs = List.update_assoc_in_place p (fun pair_info -> {pair_info with count = pair_info.count + count}) {polymers = []; produce=("","");increment="";count=(-1)} pairs

let process_template (t,pairs) =
  let pairs_bound =String.length t - 2 in
  List.fold_left (fun (counts, pairs) i ->
    let p = String.sub t i 1 in
    let counts = List.update_assoc_in_place p succ 1 counts in
    let counts = if i = pairs_bound then List.update_assoc_in_place (String.sub t (i+1) 1) succ 1 counts else counts in
    let pair = if i = pairs_bound then (Str.string_after t i) else (String.sub t i 2) in
    (counts, update_pair_counts pair 1 pairs)
  ) ([], pairs) (0--pairs_bound)

let step (counts, pairs) = List.fold_left(fun (counts, pairs) (p, pair_info) ->
    match pair_info.produce with (a,b) -> 
      let count = pair_info.count in
      let pairs = update_pair_counts a count pairs |> update_pair_counts b count |> update_pair_counts p (-count) in
      let counts = List.update_assoc_in_place pair_info.increment (fun v -> v + pair_info.count) pair_info.count counts in
      (counts, pairs)
  ) (counts, pairs) (List.filter (fun (_, pair_info) -> pair_info.count > 0) pairs)

let steps n (counts, pairs) = List.fold_left (fun (counts, pairs) _ -> 
  step (counts, pairs)) (counts, pairs) (1--n) |> fun (counts, _) -> counts

let sub_quantities counts = List.fold_left (fun (max, min) (_,count) -> (Int.max count max, Int.min count min)) (Int.min_int, Int.max_int) counts
  |> fun (max, min) -> max - min

let run l n = parse_input l 
  |> process_template
  |> steps n
  |> sub_quantities

let run_p1 l = run l 10

let run_p2 l = run l 40

let run input =
  let t1 = Sys.time () in
  let part_1 = run_p1 input in
  let d_1 = Sys.time () -. t1 in
  let t2 = Sys.time () in
  let part_2 = run_p2 input in
  let d_2 = Sys.time () -. t2 in
  Printf.printf "Day 14 - Part 1: %d in %fs\n" part_1 d_1;
  Printf.printf "Day 14 - Part 2: %d in %fs\n" part_2 d_2
