open Utils.Extensions

type octopus = { energy: int; flashed: bool}
type coordinate = { x: int; y: int}
type iteration = { flashes: int; coord : coordinate; matrix: octopus array array}

let directions = [ 
  { x = -1; y = 0}; { x = 1; y = 0}; { x = 0; y = -1}; { x = 0; y = 1}; 
  { x = -1; y = -1}; { x = -1; y = 1}; { x = 1; y = -1}; { x = 1; y = 1}
]

let adjacents_values coord m = 
  List.fold_left (fun acc d -> 
    let x = coord.x + d.x in
    let y = coord.y + d.y in
    if y > -1 && y < Array.length m && x > -1 && x < Array.length m.(0) then acc @ [{ x = x; y = y }] else acc
  ) [] directions

let to_octopus_array = Array.map (fun line -> 
  Array.map (fun o -> { energy = o; flashed = false}) line
)

let raise_energy coord m = 
  let o = m.(coord.y).(coord.x) in
  if not(o.flashed) then
    let e = o.energy + 1 in
    m.(coord.y).(coord.x) <- { energy = e; flashed = e > 9 };
    e > 9
  else false

let rec propagate coord m = 
  List.fold_left (fun flashes adj_coord -> 
    if raise_energy adj_coord m then flashes + 1 + propagate adj_coord m else flashes
  ) 0 (adjacents_values coord m)

let reset_matrix m = Array.iteri (fun y line -> Array.iteri(fun x o -> m.(y).(x) <- { energy = if o.energy > 9 then 0 else o.energy; flashed = false}) line) m

let new_step m =
  reset_matrix m;
  List.fold_left (fun flashed y -> 
    List.fold_left (fun flashed x -> 
      let coord = { x = x; y = y} in if raise_energy coord m then flashed @ [coord] else flashed
    ) flashed (0--(Array.length m.(y) - 1))
  ) [] (0--(Array.length m - 1))
  |> fun flashed -> List.fold_left (fun flashes coord -> flashes + propagate coord m) (List.length flashed) flashed

let count_nb_steps_flashes nb m = List.fold_left (fun acc _ -> acc + new_step m) 0 (1--nb)

let step_simultaneously_flash m = 
  let cave_size = Int.mul (Array.length m) (Array.length m.(0)) in
  let flashes = ref (new_step m) in
  let step = ref 1 in
  while !flashes != cave_size do
    flashes := new_step m;
    step := !step + 1;
  done;
  !step
  
let run_p1 m = count_nb_steps_flashes 100 (to_octopus_array m)

let run_p2 m = step_simultaneously_flash (to_octopus_array m)

let run input =
  let t1 = Sys.time () in
  let part_1 = run_p1 input in
  let d_1 = Sys.time () -. t1 in
  let t2 = Sys.time () in
  let part_2 = run_p2 input in
  let d_2 = Sys.time () -. t2 in
  Printf.printf "Day 11 - Part 1: %d in %fs\n" part_1 d_1;
  Printf.printf "Day 11 - Part 2: %d in %fs\n" part_2 d_2
