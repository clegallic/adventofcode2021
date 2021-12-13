open Utils.Extensions

let is_small_cave s = let c = s.[0] in let i = int_of_char c in not(65 <= i && i <= 90);;

let to_connection s = match String.split_on_char '-' s with | hd::[tl] -> (hd, tl) | _ -> failwith "Invalid input"

let to_connections = 
  let add_conn a b l = match List.assoc_opt a l with
  | Some outputs -> (List.remove_assoc a l) @ [(a, outputs @ [b])]
  | None -> l @ [(a, [b])] in
  List.fold_left (fun acc s -> let (i, o) = to_connection s in add_conn i o acc |> add_conn o i) []

let rec crawl cave current_path good_paths allow_twice has_twice connections = 
  List.fold_left (fun acc o -> 
    match o with
    | "start" -> acc
    | "end" -> acc + 1
    | _ -> 
      let already_crawled = List.mem o current_path in
      let can_crawl = if is_small_cave o then (if already_crawled then allow_twice && not(has_twice) else true) else true in
      let has_twice = has_twice || (can_crawl && already_crawled && is_small_cave o) in
      if can_crawl then crawl o (current_path @ [cave]) acc allow_twice has_twice connections else acc
  ) good_paths (List.assoc cave connections)

let run_p1 l = let connections = to_connections l in crawl "start" [] 0 false false connections

let run_p2 l = let connections = to_connections l in crawl "start" [] 0 true false connections

let run input =
  let t1 = Sys.time () in
  let part_1 = run_p1 input in
  let d_1 = Sys.time () -. t1 in
  let t2 = Sys.time () in
  let part_2 = run_p2 input in
  let d_2 = Sys.time () -. t2 in
  Printf.printf "Day 12 - Part 1: %d in %fs\n" part_1 d_1;
  Printf.printf "Day 12 - Part 2: %d in %fs\n" part_2 d_2
