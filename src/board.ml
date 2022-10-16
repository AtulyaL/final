open Pieces
open Game

(** Initiates the chess board and maintains it as the two players play*)

type tile =
  | Empty of int * int
  | Full of int * int * Pieces.t

type board = tile list

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
          | (u1, u2, _), (v1, v2) -> u1 = v1 && u2 = v2)
        occupied
    in
    match found with
    | _, _, t -> t.color <> color
  with Not_found -> true

let update_board board move piece = Raise "not implemented yet"
