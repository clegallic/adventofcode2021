open Utils.Extensions

let rec count_increased l =
  let a, b, tail = (List.hd l, List.nth_opt l 1, List.tl l) in
  match b with
  | None -> 0
  | Some b ->
      let c = count_increased tail in
      if a < b then c + 1 else c

let count_increased_windowed l =
  List.sub 0 (List.length l - 3) l
  |> List.mapi (fun i a -> a + List.nth l (i + 1) + List.nth l (i + 2))
  |> count_increased

let run input =
  let l = List.str_to_int input in
  let part_1 = count_increased l in
  let part_2 = count_increased_windowed l in
  print_endline ("Day 01 - Part 1: " ^ Int.to_string part_1);
  print_endline ("Day 01 - Part 2: " ^ Int.to_string part_2);
