open Utils.Extensions

let parse_input l = 
  let parse_dot line = let def = String.split_on_char ',' line in (int_of_string (List.nth def 1), int_of_string (List.nth def 0)) in
  let parse_fold line = let fold_def = String.split_on_char '=' (List.nth (String.split_on_char ' ' line) 2) in 
    (List.nth fold_def 0 , int_of_string (List.nth fold_def 1)) in
  let get_bounds dots = List.fold_left (fun (max_y,max_x) (y,x) -> (max max_y (y + 1), max max_x (x + 1)) ) (0,0) dots in
  List.fold_left (fun (is_dot_def, dots, folds) line -> 
    match is_dot_def with
    | true -> if line = "" then (false, dots, folds) else (true, dots @ [parse_dot line], folds)
    | false -> (false, dots, folds @ [parse_fold line])
  ) (true, [], []) l
  |> fun (_, dots, folds) -> 
    let (b_y, b_x) = get_bounds dots in
    let sheet = Array.make_matrix (b_y) (b_x) false in
    List.iter (fun (y,x) -> sheet.(y).(x) <- true) dots;
    (sheet, folds)

let do_fold (axis, offset) sheet = 
  let (b_y, b_x) = Array.y_x_bounds sheet in
  let (bfs_y, bfs_x) = if axis = "x" then (b_y, offset - 1) else (offset - 1, b_x) in
  let folded_sheet = Array.make_matrix (bfs_y + 1) (bfs_x + 1) false in
  List.iter (fun x -> List.iter ( fun y -> 
    if axis = "x" then folded_sheet.(y).(x) <- sheet.(y).(x) || sheet.(y).(b_x - x)
    else folded_sheet.(y).(x) <- sheet.(y).(x) || sheet.(b_y - y).(x)
  ) (0--bfs_y)) (iter_x folded_sheet);
  folded_sheet

let count_dots = Array.fold_left (fun acc_y line -> Array.fold_left (fun acc_x has_dot -> if has_dot then acc_x + 1 else acc_x) acc_y line) 0

let print_sheet sheet = Array.print_matrix sheet (fun has_dot -> if has_dot then "#" else ".")

let run_p1 l = parse_input l 
  |> fun (sheet, folds) -> do_fold (List.nth folds 0) sheet
  |> count_dots

let run_p2 l = parse_input l 
  |> fun (sheet, folds) -> List.fold_left (fun acc fold -> do_fold fold acc) sheet folds
  |> fun sheet -> print_sheet sheet; -1

let run input =
  let t1 = Sys.time () in
  let part_1 = run_p1 input in
  let d_1 = Sys.time () -. t1 in
  let t2 = Sys.time () in
  let part_2 = run_p2 input in
  let d_2 = Sys.time () -. t2 in
  Printf.printf "Day 13 - Part 1: %d in %fs\n" part_1 d_1;
  Printf.printf "Day 13 - Part 2: %d in %fs\n" part_2 d_2
