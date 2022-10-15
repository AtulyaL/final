open Pieces
open Game
 
(** Initiates the chess board and maintains it as the two players play*)
 
type tile =
| Empty of int * int
| Full of int * int * Pieces
 
type board = tile list
 
let is_occupied board tile (result : bool) = Raise ("not implemented yet")

let update_board board move piece = Raise ("not implemented yet")
