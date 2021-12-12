open Stdio 
open Extensions

let load_as_list filepath  =
  In_channel.with_file filepath ~f:(fun file -> In_channel.input_lines file)

let load_as_int_list filepath =
  In_channel.with_file filepath ~f:(fun file -> In_channel.input_lines file) 
  |> fun l -> String.split_on_char ',' (List.hd l)
  |> List.map int_of_string

let load_as_matrix filepath = 
  let l = load_as_list filepath in
  let m = Array.make_matrix (List.length l) (String.length (List.nth l 0)) 0 in
  List.iteri (fun y s -> List.iteri (fun x v -> m.(y).(x) <- v) (Str.split_to_int s)) l;
  m