open! Utils.Extensions

type packet_type = Literal | Operator | Unknown
type packet = { version: int; id: int; t: packet_type; sub_packets: packet list; value: int}
let type_of i = match i with | 4 -> Literal | _ -> Operator
let type_to_string = function Literal -> "Literal" | Operator-> "Operator" | Unknown-> "Unknown"

let rec print_packet p = 
  Printf.printf "(v: %d, id: %d, type: %s, value: %d)\n" p.version p.id (type_to_string p.t) p.value;
  if p.sub_packets != [] then
    Printf.printf "%d subpacket --> \n" @@ List.length p.sub_packets;
    List.iter(fun sp -> print_string "\t";print_packet sp) p.sub_packets

let bin_to_string = List.int_join ~s:""

let rec int_to_bits_list ?l:(cl=[]) i = 
  let l = [i mod 2] @ cl in
  match i / 2 with
  | 0 -> l
  | x -> int_to_bits_list x ~l:l

let rec pad4 l = if List.length l > 3 then l else pad4 @@ [0] @ l

let hexstring_to_bits h = pad4 @@ int_to_bits_list @@ int_of_string @@ "0x" ^ h

let input_to_bits i = Str.split_to_char i |> List.fold_left(fun acc h -> acc @ hexstring_to_bits h) []

let bit_list_to_int l = let l = List.rev l in List.fold_left(fun acc i -> acc + (List.nth l i) * pow 2 i) 0 (0--(List.length l - 1))

let is_packet = List.exists (fun v -> v = 1)

let rec parse_packets ?limit:(limit=Int.max_int) bin_packets f = 
  if limit = 0 || not(is_packet bin_packets) then 
    ([], bin_packets) 
  else 
    let (packet, remaining) = f bin_packets in
    let (next_packets, remaining) = parse_packets ~limit:(limit-1) remaining f in
    ([packet] @ next_packets, remaining)

let rec parse_literal_values bits = 
  match bits with
    | 1::b::c::d::e::tl -> 
      let (values, remains) = parse_literal_values tl in
      ([b;c;d;e] @ values, remains)
    | _::b::c::d::e::tl -> ([b;c;d;e],tl)
    | _ -> failwith "Bad literal input"

let parse_operator body f = 
  match body with
    | 0::tl -> 
      let subpackets_length = bit_list_to_int @@ List.sub 0 14 tl in
      let subpackets_bits = List.sub 15 (14 + subpackets_length) tl in
      let remaining_bits = if (15 + subpackets_length) < (List.length tl - 1) then List.sub (15 + subpackets_length) (List.length tl - 1) tl else [] in
      let (sub_packets, remaining) = parse_packets subpackets_bits f in
      (sub_packets, remaining @ remaining_bits)
    | 1::tl -> 
      let nb_subpackets = bit_list_to_int @@ List.sub 0 10 tl in
      let subpackets_bits = List.sub 11 (List.length tl - 1) tl in
      parse_packets subpackets_bits f ~limit:nb_subpackets
    | _ -> failwith "Bad operator"

let parse_literals body =  
  let (value, remaining) = parse_literal_values body in
  (bit_list_to_int value, remaining)

let rec parse_packet bin_packet = 
  match bin_packet with
    | v1::v2::v3::t1::t2::t3::body -> 
      begin
        let v = bit_list_to_int [v1;v2;v3] in
        let ti = bit_list_to_int [t1;t2;t3] in
        let packet = {version = v; id = ti; t = type_of ti; sub_packets = []; value = 0 } in
        let (packet, remaining) = match packet.t with
        | Literal -> let (v,r) = parse_literals body in ({packet with value = v}, r) 
        | Operator -> let (sp,r) = parse_operator body parse_packet in ({packet with sub_packets = sp}, r)
        | _ -> failwith "Unknow packet type" in
        (packet, remaining)
      end
    | _ -> failwith "Bad input"

let rec sum_versions packet = packet.version + List.fold_left(fun acc p -> acc + sum_versions p) 0 packet.sub_packets

let greater_than a b = if a = -1 then b else if a > b then 1 else 0
let less_than a b = if a = -1 then b else if a < b then 1 else 0
let equals a b = if a = -1 then b else if Int.equal a b then 1 else 0

let rec compute packet =
  if packet.id = 4 then packet.value else
  let (operator, init) = match packet.id with
    | 0 -> (Int.add, 0)
    | 1 -> (Int.mul, 1)
    | 2 -> (Int.min, Int.max_int)
    | 3 -> (Int.max, Int.min_int)
    | 5 -> (greater_than, -1)
    | 6 -> (less_than, -1)
    | 7 -> (equals, -1)
    | _ -> failwith "Bad operator"
    in
  List.fold_left (fun acc sp -> operator acc @@ compute sp) init packet.sub_packets

let run_for_line line = print_endline line; line
  |> input_to_bits
  |> parse_packet

let run_p1 input = run_for_line @@ List.nth input 0 
  |> fun (packet, _) -> sum_versions packet

let run_p2 input = run_for_line @@ List.nth input 0
  |> fun (packet, _) -> compute packet

let run input = 
  let t1 = Sys.time () in
  let part_1 = run_p1 input in
  let d_1 = Sys.time () -. t1 in
  let t2 = Sys.time () in
  let part_2 = run_p2 input in
  let d_2 = Sys.time () -. t2 in
  Printf.printf "Day 16 - Part 1: %d in %fs\n" part_1 d_1;
  Printf.printf "Day 16 - Part 2: %d in %fs\n" part_2 d_2
