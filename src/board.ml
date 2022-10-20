open Pieces

(** Initiates the chess board and maintains it as the two players play*)

type tile =
  | Empty of int * int
  | Full of int * int * Pieces.t

type board = tile list

let rec empty_init_row (x : int) (y : int) : board =
  match x <= 8 with
  | false -> []
  | true -> Empty (x, y) :: empty_init_row (x + 1) y

let rec empty_init_col (y : int) : board =
  match y <= 6 with
  | false -> []
  | true -> empty_init_row 1 y @ empty_init_col (y + 1)

let rec pawn_init_white (x : int) : board =
  match x <= 8 with
  | false -> []
  | true ->
      Full (x, 2, Pieces.init "pawn" "white" (x, 2)) :: pawn_init_white (x + 1)

let rec pawn_init_black (x : int) : board =
  match x <= 8 with
  | false -> []
  | true ->
      Full (x, 7, Pieces.init "pawn" "black" (x, 7)) :: pawn_init_black (x + 1)

let pieces (color : string) : board =
  if color = "white" then
    [
      Full (1, 1, Pieces.init "rook" "white" (1, 1));
      Full (8, 1, Pieces.init "rook" "white" (8, 1));
      Full (2, 1, Pieces.init "knight" "white" (2, 1));
      Full (7, 1, Pieces.init "knight" "white" (7, 1));
      Full (3, 1, Pieces.init "bishop" "white" (3, 1));
      Full (6, 1, Pieces.init "bishop" "white" (6, 1));
      Full (4, 1, Pieces.init "queen" "white" (4, 1));
      Full (5, 1, Pieces.init "king" "white" (5, 1));
    ]
  else
    [
      Full (1, 1, Pieces.init "rook" "black" (1, 8));
      Full (8, 1, Pieces.init "rook" "black" (8, 8));
      Full (2, 1, Pieces.init "knight" "black" (2, 8));
      Full (7, 1, Pieces.init "knight" "black" (7, 8));
      Full (3, 1, Pieces.init "bishop" "black" (3, 8));
      Full (6, 1, Pieces.init "bishop" "black" (6, 8));
      Full (4, 1, Pieces.init "queen" "black" (4, 8));
      Full (5, 1, Pieces.init "king" "black" (5, 8));
    ]

let init =
  pawn_init_black 1 @ pawn_init_white 1 @ pieces "black" @ pieces "white"
  @ empty_init_col 6

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

let rec to_string_heavy_lifter (r : int) (c : int) board : string =
  if r > 8 && c > 8 then ""
  else if c = 8 then "\n" ^ to_string_heavy_lifter (r + 1) 0 board
  else find (r, c) board ^ to_string_heavy_lifter r (c + 1) board

let to_string board = to_string_heavy_lifter 0 0 board
