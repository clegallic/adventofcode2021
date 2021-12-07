open Stdio
let load_as_list filepath  =
  In_channel.with_file filepath ~f:(fun file -> In_channel.input_lines file)

  let load_as_int_list filepath =
    In_channel.with_file filepath ~f:(fun file -> In_channel.input_lines file) 
    |> fun l -> String.split_on_char ',' (List.hd l)
    |> List.map int_of_string