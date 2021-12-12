module List = struct
  include List

  let reduce f = function
    | x :: xs -> List.fold_left f x xs
    | [] -> failwith "No empty list"
  ;;

  let str_to_int l = List.map int_of_string l
  let int_to_str l = List.map string_of_int l
  let sum la lb = List.mapi (fun i a -> List.nth lb i + a) la

  let rec sub b e l =
    match l with
    | [] -> failwith "sublist"
    | h :: t ->
      let tail = if e = 0 then [] else sub (b - 1) (e - 1) t in
      if b > 0 then tail else h :: tail
  ;;

  let int_join l = String.concat " " (List.map string_of_int l)

  let rec index_of x ?(c = 0) lst =
    match lst with
    | [] -> -1
    | hd :: tl -> if hd = x then c else index_of x tl ~c:(c + 1)
  ;;

  let rec fold_n n f acc =
    if n <= 0 then acc else fold_n (pred n) f (f acc)

  let min l = List.fold_left (fun acc v -> if v < acc then v else acc ) Int.max_int l

  let max l = List.fold_left (fun acc v -> if v > acc then v else acc ) Int.min_int l

  (** All keys associated with this value (ssociation list of pairs)**)
  let rec assoc_value_all v1 l = match l with
  | [] -> []
  | (k,v)::tl -> if v1 = v then [k] @ assoc_value_all v1 tl else assoc_value_all v1 tl

  let rec flat_map f xs =
  match xs with
  | [] -> []
  | x :: xs -> List.append (f x) (flat_map f xs);;

  let product lists =
    let rec loop acc lists =
      match lists with
      | [] -> [[]]
      | first :: [] -> first |> List.map (fun x -> x :: acc)
      | first :: rest -> first |> flat_map (fun x -> loop (x :: acc) rest)
    in
      loop [] lists;;

  let rec merge s t =
    match s, t with
    | x :: s , [] | [], x :: s -> x :: s
    | [], [] -> s
    | x :: s', y :: t' ->
      if x < y then
        x :: (merge s' t)
      else if x = y then
        x :: (merge s' t')
      else
          y :: (merge s t')

  let rec last = function
  | [] -> failwith "Empty list"
  | [x] -> x
  | _ :: t -> last t

  let remove_last l = match l with 
    | [_] -> []
    | _ -> sub 0 (List.length l - 2) l

end

let pow x y = float_of_int x ** float_of_int y |> int_of_float

module Str = struct
  include Str

  let split_to_char s = Str.split (Str.regexp "") s

  let split_to_int ?delim:(d = "") s =
    Str.split (Str.regexp d) s |> List.map (fun a -> int_of_string a)
end

module Array = struct
  include Array

  let print_matrix m =
    for y = 0 to Array.length m - 1 do
      for x = 0 to Array.length m.(0) - 1 do
        let v = m.(y).(x) in
        Printf.printf "%d" v;
      done;
      print_newline ()
    done
  ;;
end

let int_of_bstring s =
  Str.split_to_char s
  |> List.map (fun c -> int_of_string c)
  |> List.rev
  |> List.mapi (fun i a -> if a = 0 then 0 else pow 2 i)
  |> List.fold_left Int.add 0
;;

let ( -- ) i j =
  let rec aux n acc = if n < i then acc else aux (n - 1) (n :: acc) in
  aux j []
;;

let print_matrix m = Array.print_matrix (Array.map (fun l -> Array.map (fun o -> o.energy) l) m); print_newline();;