open Utils.Extensions

let line_re = Str.regexp "\\([0-9]+\\),\\([0-9]+\\) -> \\([0-9]+\\),\\([0-9]+\\)"

let to_line s =
  match Str.string_match line_re s 0 with
  | true ->
    ( (int_of_string (Str.matched_group 1 s), int_of_string (Str.matched_group 2 s))
    , (int_of_string (Str.matched_group 3 s), int_of_string (Str.matched_group 4 s)) )
  | false -> failwith "Wrong input"
;;

let off v = if v = 0 then 0 else v / abs v

let rec draw ((x1, y1), (x2, y2)) m =
  let x_off = off (x2 - x1) in
  let y_off = off (y2 - y1) in
  m.(y1).(x1) <- m.(y1).(x1) + 1;
  if not ((x1, y1) = (x2, y2)) then draw ((x1 + x_off, y1 + y_off), (x2, y2)) m
;;

let bounds l =
  List.fold_left
    (fun (xmax, ymax) ((x1, y1), (x2, y2)) ->
      ( (if x1 > xmax then x1 else if x2 > xmax then x2 else xmax)
      , if y1 > ymax then y1 else if y2 > ymax then y2 else ymax ))
    (0, 0)
    l
;;

let run f l =
  List.map to_line l
  |> fun lines ->
  let xmax, ymax = bounds lines in
  let m = Array.make_matrix (ymax + 1) (xmax + 1) 0 in
  lines |> f |> List.iter (fun line -> draw line m);
  Array.fold_left
    (fun acc row ->
      Array.fold_left (fun accr v -> if v >= 2 then accr + 1 else accr) acc row) 0 m
;;

let run_p1 l =
  run (fun l -> List.filter (fun ((x1, y1), (x2, y2)) -> x1 = x2 || y1 = y2) l) l
;;

let run_p2 l = run (fun l -> l) l

let run input =
  let part_1 = run_p1 input in
  let part_2 = run_p2 input in
  print_endline ("Day 05 - Part 1: " ^ Int.to_string part_1);
  print_endline ("Day 05 - Part 2: " ^ Int.to_string part_2)
;;
