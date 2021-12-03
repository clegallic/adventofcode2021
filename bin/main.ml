
let input_name = "input.txt"
let day01 = Utils.Input.load_as_list ("day01/" ^ input_name) |> Day01.Main.run
let day02 = Utils.Input.load_as_list ("day02/" ^ input_name) |> Day02.Main.run
let day03 = Utils.Input.load_as_list ("day03/" ^ input_name) |> Day03.Main.run

let () = day03
