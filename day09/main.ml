open Utils.Extensions

type offset = Offset of int * int
let directions = [ Offset (-1, 0); Offset (1, 0); Offset (0,-1); Offset (0,1)]

let to_matrix l = let m = Array.make_matrix (List.length l) (String.length (List.nth l 0)) 0 in
  List.iteri (fun y s -> List.iteri (fun x v -> m.(y).(x) <- v) (Str.split_to_int s)) l;
  m

let adjacents_values (y,x) map = 
  let up = if y = 0 then Int.max_int else map.(y-1).(x) in
  let down = if y = (Array.length map - 1) then Int.max_int else map.(y+1).(x) in
  let left = if x = 0 then Int.max_int else map.(y).(x - 1) in
  let right = if x = (Array.length map.(y) - 1) then Int.max_int else map.(y).(x + 1) in
  (up, down, left, right)

let bounds a = (Array.length a, Array.length a.(0))

let find_low_points map = 
  let (by, bx) = bounds map in
  List.fold_left (fun accy y -> 
    let for_x = List.fold_left (fun accx x -> 
      let c = map.(y).(x) in
      let (up, down, left, right) = adjacents_values (y, x) map in
      if (c < up && c < down && c < left && c < right) then accx @ [(y, x, c)] else accx
    ) [] (0--(bx - 1)) in
  accy @ for_x
  ) [] (0--(by - 1))

let rec crawl (y,x) map visited =
  let visited = visited @ [(y,x)] in
  let (by, bx) = bounds map in
  List.fold_left (fun visited (Offset (oy, ox)) ->
    let (cy, cx) = (y + oy, x + ox) in
    if cy < 0 || cx < 0 || cy = by || cx = bx || map.(cy).(cx) = 9 then visited else
    if List.exists (fun v -> v = (cy,cx)) visited then visited else
    crawl (cy, cx) map visited
  ) visited directions

let basin_size (y, x) map = crawl (y,x) map [] |> List.length

let run_p1 l = let map = to_matrix l in
  find_low_points map
  |> List.map (fun (_,_,v) -> v + 1)
  |> List.fold_left Int.add 0
  
let run_p2 l = let map = to_matrix l in
  find_low_points map
  |> List.map (fun (y, x, _) -> basin_size (y,x) map)
  |> List.sort (fun a b -> b - a)
  |> List.sub 0 2
  |> List.fold_left Int.mul 1

let run input =
  let t1 = Sys.time () in
  let part_1 = run_p1 input in
  let d_1 = Sys.time () -. t1 in
  let t2 = Sys.time () in
  let part_2 = run_p2 input in
  let d_2 = Sys.time () -. t2 in
  Printf.printf "Day 09 - Part 1: %d in %fs\n" part_1 d_1;
  Printf.printf "Day 09 - Part 2: %d in %fs\n" part_2 d_2
