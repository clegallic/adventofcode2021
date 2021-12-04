open Utils.Extensions

type board = { rows : int list list; cols : int list list; first_wins_at:int; index : int }
type win_with = Row | Column | None
type winner = {
  board_index : int;
  wins_with: win_with;
  win_number_pos : int;
}
type result = { boards : board list; current_board : board; winner : winner }

let row_wins row numbers winner =
  List.fold_left
    (fun last n ->
      let n_pos = List.index_of n numbers in
      if n_pos > last then n_pos else last)
    0 row
  |> fun last ->
  if last < winner.win_number_pos then ((true, last)) else (false, last)

let board_col_wins board numbers winner = 
  List.fold_left (fun acc col -> match row_wins col numbers winner with wins, pos -> match acc with (pwins, ppos) -> (pwins || wins, if pos < ppos then pos else ppos) )
    (false, Int.max_int) board.cols

let new_board i = { rows = []; cols = List.init 5 (fun _ -> []); first_wins_at = Int.max_int; index = i }

let new_winner board wins_with win_number_pos = {
  board_index = board.index;
  wins_with = wins_with;
  win_number_pos = win_number_pos;
}

let lowest a b = if a < b then a else b

let find_winner numbers all_rows =
  let fake_winner =
    {
      board_index = -1;
      wins_with = None;
      win_number_pos = Int.max_int;
    }
  in
  let result =
    {
      boards = [];
      current_board = new_board 0;
      winner = fake_winner;
    }
  in
  let all_rows = all_rows @ [""] in
  List.fold_left
    (fun result row ->
      match row with
      | "" ->
            let (wins, win_number_pos) = board_col_wins result.current_board numbers result.winner in
            let board = {
              result.current_board with first_wins_at = lowest win_number_pos result.current_board.first_wins_at;
            } in
            {
              boards = result.boards @ [ board ];
              current_board = new_board (board.index + 1);
              winner = match wins, win_number_pos with
              | true, win_number_pos -> new_winner result.current_board Column win_number_pos
              | false, _ -> result.winner
            }
      | _ -> (
          let row_int = Str.split_to_int ~delim:" +" row in
          let updated_board = {
            result.current_board with
            rows = result.current_board.rows @ [ row_int ];
            cols = List.mapi (fun i v -> (List.nth result.current_board.cols i) @ [v] ) row_int;
            index = result.current_board.index;
          } in
          match row_wins row_int numbers result.winner with
          | true, win_number_pos ->
              { result with
                current_board = { updated_board with first_wins_at = lowest win_number_pos updated_board.first_wins_at};
                winner = new_winner result.current_board Row win_number_pos
              }
          | false, win_number_pos ->
              { result with
                current_board = { updated_board with first_wins_at = lowest win_number_pos updated_board.first_wins_at}
              }))
    result all_rows

let has_been_called n called_numbers = not (List.index_of n called_numbers = -1)

let sum_of_unmarked_numbers board called_numbers =
  List.fold_left
    (fun sum row ->
      sum
      + List.fold_left
          (fun acc n ->
            if has_been_called n called_numbers then acc else acc + n)
          0 row)
    0 board.rows

let bingo f l = 
  match l with
  | n :: _ :: boards_lines ->
      let numbers = Str.split_to_int ~delim:"," n in
      find_winner numbers boards_lines 
        |> fun result -> let (win_number_pos, board_index) = f result in
      let called_numbers = List.sub 0 win_number_pos numbers in
      sum_of_unmarked_numbers
        (List.nth result.boards board_index)
        called_numbers
      * List.nth numbers win_number_pos
  | _ -> failwith "Bad input"
let run_p1 l = bingo (fun result -> result.winner.win_number_pos, result.winner.board_index) l

let run_p2 l = bingo (fun result -> 
  List.sort (fun b1 b2 -> b2.first_wins_at - b1.first_wins_at) result.boards 
  |> List.hd 
  |> fun board -> (board.first_wins_at, board.index)
  ) l

let run input =
  let part_1 = run_p1 input in
  let part_2 = run_p2 input in
  print_endline ("Day 04 - Part 1: " ^ Int.to_string part_1);
  print_endline ("Day 04 - Part 2: " ^ Int.to_string part_2)
