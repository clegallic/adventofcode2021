module List = struct
  include List

  let reduce f = function
    | x :: xs -> List.fold_left f x xs
    | [] -> failwith "No empty list"

  let str_to_int l = List.map int_of_string l

  let int_to_str l = List.map string_of_int l

  let sum la lb = List.mapi (fun i a -> List.nth lb i + a) la

  let rec sub b e l =
    match l with
    | [] -> failwith "sublist"
    | h :: t ->
        let tail = if e = 0 then [] else sub (b - 1) (e - 1) t in
        if b > 0 then tail else h :: tail
end

let pow x y = float_of_int x ** float_of_int y |> int_of_float

module Str = struct
  include Str

  let split_to_char s = Str.split (Str.regexp "") s

  let split_to_int s =
    Str.split (Str.regexp "") s |> List.map (fun a -> int_of_string a)
end

let int_of_bstring s =
  Str.split_to_char s
  |> List.map (fun c -> int_of_string c)
  |> List.rev
  |> List.mapi (fun i a -> if a = 0 then 0 else pow 2 i)
  |> List.fold_left Int.add 0
