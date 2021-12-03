open Utils.Extensions

let bits_to_int l = "0b" ^ String.concat "" (List.int_to_str l) |> int_of_string

let most_or_least s l a b =
  List.map (fun c -> if c < List.length l / 2 then a else b) s

let most s l = most_or_least s l 0 1

let least s l = most_or_least s l 1 0

let run_p1 l =
  List.map (fun a -> Str.split_to_int a) l
  |> List.reduce (fun acc a -> List.sum acc a)
  |> fun s -> bits_to_int (most s l) * bits_to_int (least s l)

let most_or_least_at i l a b =
  List.fold_left (fun acc a -> acc + List.nth a i) 0 l |> fun s ->
  if List.length l - s > s then a else b

let rec keep_at_if i f l =
  let m = f i l in
  List.filter (fun a -> List.nth a i = m) l |> fun lf ->
  match lf with [ a ] -> a | _ -> keep_at_if (i + 1) f lf

let oxygen l = keep_at_if 0 (fun i l -> most_or_least_at i l 0 1) l

let co2 l = keep_at_if 0 (fun i l -> most_or_least_at i l 1 0) l

let run_p2 l =
  List.map (fun a -> Str.split_to_int a) l 
  |> fun a -> (oxygen a, co2 a) 
  |> fun (o, c) -> bits_to_int o * bits_to_int c

let run input =
  let part_1 = run_p1 input in
  let part_2 = run_p2 input in
  print_endline ("Day 03 - Part 1: " ^ Int.to_string part_1);
  print_endline ("Day 03 - Part 2: " ^ Int.to_string part_2)
