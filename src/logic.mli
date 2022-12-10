type direction =
  | Left
  | Right  (** [direction] is the direction in which the king is castling*)

exception Castle of direction * Pieces.t
(** Raised to indicate that a king is castling*)

val check_move : int * int -> Pieces.t -> Board.board -> Pieces.zcolor -> bool
(* check_move takes in a move, a piece, a board, and a color and then returns a
   bool called status which checks if the piece can move *)

val check : Board.board -> Pieces.zcolor -> bool
(* check takes in a board and returns whether or not any side is in check *)

val check_mate : Board.board -> Pieces.zcolor -> bool
(* checkmate takes in a board and returns whether or not any side is in
   checkmate *)

val stalemate : Board.board -> bool