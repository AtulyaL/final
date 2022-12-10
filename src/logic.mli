(* type status *)
(* type status type status is a record containing the status of the game; its
   fields are "white", "black", and "turn", which is whether or not each side is
   in check and whose turn it is *)

(* val beg : status *)
(* beg is the current status of the game*)

type direction =
  | Left
  | Right

exception Castle of direction * Pieces.t

val check_move : int * int -> Pieces.t -> Board.board -> Pieces.zcolor -> bool
(* check_move takes in a move, a piece, a board, and a color and then returns a
   bool called status which checks if the piece can move *)

val check : Board.board -> Pieces.zcolor -> bool
(* check takes in a board and returns whether or not any side is in check *)

val check_mate : Board.board -> Pieces.zcolor -> bool
(* checkmate takes in a board and returns whether or not any side is in
   checkmate *)

val stalemate : Board.board -> bool