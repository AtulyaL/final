(* type status *)
(* type status type status is a record containing the status of the game; its
   fields are "white", "black", and "turn", which is whether or not each side is
   in check and whose turn it is *)

(* val beg : status *)
(* beg is the current status of the game*)

val check_move : int * int -> Pieces.t -> Board.board -> Pieces.color -> bool
(* check_move takes in a move, a piece, a board, and a color and then returns a
   bool called status which checks if the piece can move *)

val check : Board.board -> Pieces.color -> bool
(* check takes in a board and returns whether or not any side is in check *)

val check_mate : Board.board -> Pieces.color -> bool
(* checkmate takes in a board and returns whether or not any side is in
   checkmate *)

(* val update_status : status -> status *)
(* update_status takes in the status of the game and returns the new status of
   the game *)

(* val check_jumps : Board.board -> Pieces.t -> bool *)
(* check_jumps sees if a piece can jump over another piece based on the current
   board*)
