module List = struct
  include List

  let str_to_int l = List.map int_of_string l

  let int_to_str l = List.map string_of_int l

  let rec sub b e l =
    match l with
    | [] -> failwith "sublist"
    | h :: t ->
        let tail = if e = 0 then [] else sub (b - 1) (e - 1) t in
        if b > 0 then tail else h :: tail
end
