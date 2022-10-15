open Pieces
open Game

(** Initiates the chess board and maintains it as the two players play*)

type board = Node of (int * int)
(** The type of the board, which is represented by a series of a node with tuple
  of two ints*)

(** *)
