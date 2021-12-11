open Utils.Extensions

type delim_info = { opening: bool; symbol : string}

let chunks_delims = [ ("(",")"); ("[","]"); ("{","}"); ("<",">")]

let grab_info symbol =  { opening = not(List.assoc_opt symbol chunks_delims = None); symbol = symbol }

let opening_symbol closing = List.find (fun (_,c) -> c = closing) chunks_delims |> fun (o, _) -> o

let closing_symbol opening = List.find (fun (o,_) -> o = opening) chunks_delims |> fun (_, c) -> c

let score_p1 symbol = match symbol with | ")" -> 3 | "]" -> 57 | "}" -> 1197 | ">" -> 25137 | _ -> 0

let score_p2 symbol = match symbol with | ")" -> 1 | "]" -> 2 | "}" -> 3 | ">" -> 4 | _ -> 0

let check_corruption line f = 
  Str.split_to_char line
  |> List.fold_left (fun (is_corrupted, reduced) symbol -> 
      if is_corrupted then (is_corrupted, reduced) else
      let symbol_info = grab_info symbol in
      if symbol_info.opening then (false, reduced @ [symbol]) else
      let last_symbol_info = grab_info (List.last reduced) in
      if last_symbol_info.symbol = opening_symbol symbol_info.symbol 
      then (false, List.remove_last reduced) else (true, reduced @ [symbol])
    ) (false, [])
  |> f

let score_chunk = fun (is_corrupted, reduced) -> (is_corrupted, score_p1 (List.last reduced))
let reduce_chunk = fun a -> a

let score_chunks = List.fold_left (
  fun acc chunk -> match check_corruption chunk score_chunk with (true, score) -> acc + score | _ -> acc ) 0 

let valid_chunks = List.fold_left (
  fun acc chunk -> match check_corruption chunk reduce_chunk with (false, reduced) -> acc @ [reduced] | _ -> acc ) [] 

let complete_chunk chunk = List.fold_right (fun symbol acc -> acc @ [closing_symbol symbol]) chunk []

let score_completion = 
  List.fold_left (fun acc s -> Int.mul acc 5 + score_p2 s) 0

let run_p1 l = score_chunks l
  
let run_p2 l = valid_chunks l 
  |> List.map complete_chunk
  |> List.map score_completion
  |> List.sort compare
  |> fun scores -> List.nth scores (List.length scores / 2)

let run input =
  let t1 = Sys.time () in
  let part_1 = run_p1 input in
  let d_1 = Sys.time () -. t1 in
  let t2 = Sys.time () in
  let part_2 = run_p2 input in
  let d_2 = Sys.time () -. t2 in
  Printf.printf "Day 10 - Part 1: %d in %fs\n" part_1 d_1;
  Printf.printf "Day 10 - Part 2: %d in %fs\n" part_2 d_2
