open Pieces

(** Initiates the chess board and maintains it as the two players play*)

type tile =
  | Empty of int * int
  | Full of int * int * Pieces.t

type board = tile list

let rec init = []

let rec pawn_init_white (x : int) : board =
  match x <= 8 with
  | false -> []
  | true ->
      Full (x, 2, Pieces.init "pawn" "white" (x, 2)) :: pawn_init_white (x + 1)

let rec pawn_init_black (x : int) : board =
  match x <= 8 with
  | false -> []
  | true ->
      Full (x, 7, Pieces.init "pawn" "white" (x, 7)) :: pawn_init_white (x + 1)

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
    | Full (_, _, t) -> t.color <> color
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
  match (piece.loc, move) with
  | (x1, y1), (x2, y2) ->
      Empty (x1, y1) :: Full (x2, y2, update_location piece (x2, y2)) :: others
