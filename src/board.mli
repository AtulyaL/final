open Pieces
open Game
 
(** Initiates the chess board and maintains it as the two players play*)
 
type `a tile =
| Empty of int * int
| Full of int * int * Pieces
(** The type of a singular tile of the board, which is either empty or a tuple
 of two integers *)
 
type board = tile list
(** The type of the board, which is represented a list of tiles *)
 
(** *)

