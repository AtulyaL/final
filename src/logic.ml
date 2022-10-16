open Pieces
open Board

(**first horizontal, then vertical for moves*)

(** checks if pawn move is valid; returns a bool*)
let pawn_move move info board = match move, info.loc with 
| (u1, u2), (t1, t2) -> if u1 = t1 then (if u2 = t2 + 1 then is_occupied board move else if u2 = t2 + 2 then is_occupied board move && not info.moved) else
  if (u1 = t1 + 1 || u1 = t1 - 1) && u2 = t2 + 1 then 

let knight_move move info board = match move with 
| (u1, u2) -> "x"

let rook_move move info board = match move with 
| (u1, u2) -> "x"

let bishop_move move info board = match move with 
| (u1, u2) -> "x"

let queen_move move info board = match move with 
| (u1, u2) -> "x"

let king_move move info board = match move with 
| (u1, u2) -> "x"


let check_move move info board = match info.name with 
| "pawn" -> pawn_move move info board
| "knight" -> knight_move move info board
| "rook" -> rook_move move info board
| "bishop" -> bishop_move move info board
| "queen" -> queen_move move info board
| "king" -> king_move move info board
| _ -> 