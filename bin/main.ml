
let input_name = "input.txt"
let day01 = Utils.Input.load_as_list ("day01/" ^ input_name) |> Day01.Main.run

let () = day01
