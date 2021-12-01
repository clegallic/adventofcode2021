open Stdio
let load_as_list filepath : 'a list =
  In_channel.with_file filepath ~f:(fun file -> In_channel.input_lines file)