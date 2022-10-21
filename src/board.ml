open Pieces

(** Initiates the chess board and maintains it as the two players play*)

type tile =
  | Empty of int * int
  | Full of int * int * Pieces.t

type board = tile list

let rec empty_init_col (x : int) (y : int) : board =
  match y <= 8 with
  | false -> []
  | true -> Empty (x, y) :: empty_init_col x (y + 1)

let rec empty_init_row (x : int) : board =
  match x <= 6 with
  | false -> []
  | true -> empty_init_col x 1 @ empty_init_row (x + 1)

let rec pawn_init_white (x : int) : board =
  match x <= 8 with
  | false -> []
  | true ->
      Full (2, x, Pieces.init "pawn" "white" (2, x)) :: pawn_init_white (x + 1)

let rec pawn_init_black (x : int) : board =
  match x <= 8 with
  | false -> []
  | true ->
      Full (7, x, Pieces.init "pawn" "black" (7, x)) :: pawn_init_black (x + 1)

let pieces (color : string) : board =
  if color = "white" then
    [
      Full (1, 1, Pieces.init "rook" "white" (1, 1));
      Full (1, 8, Pieces.init "rook" "white" (1, 8));
      Full (1, 2, Pieces.init "knight" "white" (1, 2));
      Full (1, 7, Pieces.init "knight" "white" (1, 7));
      Full (1, 3, Pieces.init "bishop" "white" (1, 3));
      Full (1, 6, Pieces.init "bishop" "white" (1, 6));
      Full (1, 4, Pieces.init "queen" "white" (1, 4));
      Full (1, 5, Pieces.init "king" "white" (1, 5));
    ]
  else
    [
      Full (8, 1, Pieces.init "rook" "black" (8, 1));
      Full (8, 8, Pieces.init "rook" "black" (8, 8));
      Full (8, 2, Pieces.init "knight" "black" (8, 2));
      Full (8, 7, Pieces.init "knight" "black" (8, 7));
      Full (8, 3, Pieces.init "bishop" "black" (8, 3));
      Full (8, 6, Pieces.init "bishop" "black" (8, 6));
      Full (8, 4, Pieces.init "queen" "black" (8, 4));
      Full (8, 5, Pieces.init "king" "black" (8, 5));
    ]

let init =
  pawn_init_black 1 @ pawn_init_white 1 @ pieces "black" @ pieces "white"
  @ empty_init_row 2

let valid_move board (move : int * int) (color : string) =
  let occupied =
    List.filter
      (fun x ->
        match x with
        | Empty (_, _) -> false
        | Full (_, _, _) -> true)
      board
  in
  try
    let found =
      List.find
        (fun x ->
          match (x, move) with
          | Full (u1, u2, _), (v1, v2) -> u1 = v1 && u2 = v2
          | _ -> false)
        occupied
    in
    match found with
    | Full (_, _, t) -> Pieces.color t <> color
    | _ -> false
  with Not_found -> true

let update_board board move piece =
  let others =
    List.filter
      (fun x ->
        match (x, move) with
        | Empty (x1, y1), (x2, y2) -> x1 <> x2 && y1 <> y2
        | Full (x1, y1, _), (x2, y2) -> x1 <> x2 && y1 <> y2)
      board
  in
  match (location piece, move) with
  | (x1, y1), (x2, y2) ->
      Empty (x1, y1) :: Full (x2, y2, update_location piece (x2, y2)) :: others

let find (coord : int * int) board : string =
  try
    let tile =
      List.find
        (fun a ->
          match (a, coord) with
          | Empty (x1, x2), (c1, c2) | Full (x1, x2, _), (c1, c2) ->
              x1 = c1 && x2 = c2)
        board
    in
    match tile with
    | Empty (_, _) -> "_"
    | Full (_, _, p) -> p |> name
  with Not_found -> (
    match coord with
    | a, b -> string_of_int a ^ string_of_int b)

let find_piece (coord : int * int) board : t =
  let tile =
    List.find
      (fun a ->
        match (a, coord) with
        | Empty (x1, x2), (c1, c2) | Full (x1, x2, _), (c1, c2) ->
            x1 = c1 && x2 = c2)
      board
  in
  match tile with
  | Empty (_, _) -> raise (Failure "No, but this can't be...")
  | Full (_, _, p) -> p

let rec to_string_heavy_lifter (r : int) (c : int) board accum : string =
  if r > 8 && c > 8 then ""
  else if c = 8 && r <> 8 then
    to_string_heavy_lifter (r + 1) 1 board ("\n" ^ accum)
  else to_string_heavy_lifter r (c + 1) board (find (r, c) board ^ accum)

let rec to_string_list (r : int) (c : int) (board : board) =
  match c > 8 with
  | true -> []
  | false -> find (r, c) board :: to_string_list r (c + 1) board

let rec to_string_rows r board =
  match r = 0 with
  | true -> []
  | false -> to_string_list r 1 board :: to_string_rows (r - 1) board

let to_lst board = to_string_rows 8 board
let to_string (board : board) = to_string_heavy_lifter 1 1 board ""
