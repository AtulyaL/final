open Pieces
open Game
 
(** Initiates the chess board and maintains it as the two players play*)
 
type tile =
| Empty of int * int
| Full of int * int * Pieces
 
type board = tile list
 

