open Utils.Extensions

exception Invalid_input

let to_instruction line =
  match String.split_on_char ' ' line with
  | [ c; o ] -> (c, int_of_string o)
  | _ -> raise Invalid_input

let move (h, d) (c, o) =
  match c with
  | "forward" -> (h + o, d)
  | "down" -> (h, d + o)
  | "up" -> (h, d - o)
  | _ -> (h, d)

let move_with_aim (h, d, a) (c, o) =
  match c with
  | "forward" -> (h + o, d + (o * a), a)
  | "down" -> (h, d, a + o)
  | "up" -> (h, d, a - o)
  | _ -> (h, d, a)

let calc_pos l f i =
  List.fold_left (fun pos ins -> f pos (to_instruction ins)) i l

let run_p1 l = calc_pos l move (0, 0) |> fun (h, d) -> h * d

let run_p2 l = calc_pos l move_with_aim (0, 0, 0) |> fun (h, d, _) -> h * d

let run input =
  let part_1 = run_p1 input in
  let part_2 = run_p2 input in
  print_endline ("Day 02 - Part 1: " ^ Int.to_string part_1);
  print_endline ("Day 02 - Part 2: " ^ Int.to_string part_2)
