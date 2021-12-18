open Core_kernel

(* Thanks to https://github.com/leviroth/advent-of-code-2021/blob/main/src/day15.ml for optimized disjkstra implementation *)

module Output = Int

module Int_pair = struct
  include Tuple.Make (Int) (Int)
  include Tuple.Comparable (Int) (Int)
  include Tuple.Hashable (Int) (Int)
  let add (a_1, b_1) (a_2, b_2) = a_1 + a_2, b_1 + b_2
  let neighbors t directions = List.map directions ~f:(add t)
  let right_vectors = [ 1, 0; 0, 1; -1, 0; 0, -1 ]
end

module Grid = struct
  type t = int array array

  let project_value value ~repetitions = ((value - 1 + repetitions) % 9) + 1

  let get t (row, col) =
    let rows = Array.length t in
    let cols = Array.length t.(0) in
    let row_repetitions = row / rows in
    let col_repetitions = col / cols in
    let original_row_index = row % rows in
    let original_col_index = col % cols in
    t.(original_row_index).(original_col_index)
    |> project_value ~repetitions:col_repetitions
    |> project_value ~repetitions:row_repetitions
  ;;
end

module Heap = Hash_heap.Make (Int_pair)

let solve input ~repetitions =
  (* matrix size *)
  let rows = Array.length input * repetitions in
  let cols = Array.length input.(0) * repetitions in
  let compare_distance a b =
    match a, b with
    | None, None -> 0
    | Some _, None -> -1
    | None, Some _ -> 1
    | Some a, Some b -> compare a b
  in
  (* Hash_heap to ensure lowest distance value is retrieved when searching for key  *)
  let heap = Heap.create ~min_size:(rows * cols) compare_distance in
  (* Fill the heap with matrix coordinate as key and 'None' as value *)
  Sequence.cartesian_product (Sequence.range 0 rows) (Sequence.range 0 cols)
  |> Sequence.iter ~f:(fun coords -> Heap.push_exn heap ~key:coords ~data:None);
  (* Replace (0,0) value with 'Some 0' *)
  Heap.replace heap ~key:(0, 0) ~data:(Some 0);
  (* Loop until reaching last position *)
  Sequence.fold_until
    (Sequence.repeat ())
    ~init:()
    ~f:(fun () () ->
      (* Get coords and risk level of first popped heap entry *)
      let coords, risk = Heap.pop_with_key_exn heap in
      let risk = Option.value_exn risk in
      match Int_pair.equal coords (rows - 1, cols - 1) with
      | true -> Continue_or_stop.Stop risk (* last cell, stop *)
      | false ->
        let neighbor_coords =
          Int_pair.neighbors coords Int_pair.right_vectors
          |> List.filter ~f:(Heap.mem heap)
        in
        (* Iterate through neighboors and set their risk level = min (neighbour_updated_risk_level, risk_level + neighbour_initial_risk_level) *)
        List.iter neighbor_coords ~f:(fun neighbor_coords ->
            let new_risk =
              Base.Comparable.min
                compare_distance
                (Heap.find_exn heap neighbor_coords)
                (Some (risk + Grid.get input neighbor_coords))
            in
            Heap.replace heap ~key:neighbor_coords ~data:new_risk);
        Continue ())
    ~finish:(fun () -> assert false)
;;

let run input = 
  let t1 = Sys.time () in
  let part_1 = solve ~repetitions:1 input in
  let d_1 = Sys.time () -. t1 in
  let t2 = Sys.time () in
  let part_2 = solve ~repetitions:5 input in
  let d_2 = Sys.time () -. t2 in
  Printf.printf "Day 15 - Part 1: %d in %fs\n" part_1 d_1;
  Printf.printf "Day 15 - Part 2: %d in %fs\n" part_2 d_2
