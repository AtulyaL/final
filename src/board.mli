type tile
(** The type of a singular tile of the board, which is either empty or a tuple
    of two integers *)

type board
(** The type of the board, which is represented a list of tiles *)

val init : board
(** Creates a starting board*)

val valid_move : board -> int * int -> string -> bool
(** Determines if a space is occupied or not based off of the given board and
    current tile*)

val update_board : board -> int * int -> Pieces.t -> board
(** Updates the board given an old board, move, and a piece *)

val find_piece : int * int -> board -> Pieces.t
(** find_piece finds the piece on the board given the location and particular
    board*)

val to_string : board -> string
(** Converts a board to a string for the command line*)