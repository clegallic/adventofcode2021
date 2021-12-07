open Utils.Extensions

let median array =
  let len = Array.length array in
  Array.sort compare array;
  (array.((len - 1) / 2) + array.(len / 2)) / 2

let run_p1 l =
  median (Array.of_list l) |> fun m ->
  List.fold_left (fun acc a -> acc + abs (a - m)) 0 l

let move l m =
  List.map
    (fun v ->
      let d = abs (v - m) in
      d * (d + 1) / 2)
    l

let run_p2 l =
  let min = List.min l in
  let max = List.max l in
  List.fold_left
    (fun acc m ->
      let lm = move l m in
      acc @ [ List.fold_left ( + ) 0 lm ])
    [] (min -- max)
  |> List.sort compare |> List.hd

let run input =
  let t1 = Sys.time () in
  let part_1 = run_p1 input in
  let d_1 = Sys.time () -. t1 in
  let t2 = Sys.time () in
  let part_2 = run_p2 input in
  let d_2 = Sys.time () -. t2 in
  Printf.printf "Day 07 - Part 1: %d in %fs\n" part_1 d_1;
  Printf.printf "Day 07 - Part 2: %d in %fs\n" part_2 d_2
