open Utils.Extensions
module Count = Map.Make (Int)

let incr ?(i = 1) v =
  match v with
  | None -> Some i
  | Some e -> Some (e + i)
;;

let grow c =
  let c0 = Count.find 0 c in
  Count.fold
    (fun k _ c1 ->
      match k with
      | 8 -> Count.add 8 0 c1
      | _ -> Count.add k (Count.find (k + 1) c1) c1) 
  c c
  |> Count.update 6 (incr ~i:c0)
  |> Count.update 8 (incr ~i:c0)
;;

let run l days =
  let count = List.fold_left (fun acc i -> Count.add i 0 acc) Count.empty (0 -- 8) in
  String.split_on_char ',' (List.hd l)
  |> List.str_to_int
  |> List.fold_left (fun acc p -> Count.update p incr acc) count
  |> fun count -> List.fold_left (fun acc _ -> grow acc) count (1 -- days)
  |> fun count -> Count.fold (fun _ v acc -> v + acc) count 0
;;

let run_p1 l = run l 80
let run_p2 l = run l 256

let run input =
  let part_1 = run_p1 input in
  let part_2 = run_p2 input in
  print_endline ("Day 06 - Part 1: " ^ Int.to_string part_1);
  print_endline ("Day 06 - Part 2: " ^ Int.to_string part_2)
;;
