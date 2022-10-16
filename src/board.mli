type tile
(** The type of a singular tile of the board, which is either empty or a tuple
    of two integers *)

type board
(** The type of the board, which is represented a list of tiles *)

val initialize : board
(** Creates a starting board*)

val is_occupied : board -> tile -> string -> bool
(** Determines if a space is occupied or not based off of the given board and
    current tile*)

val update_board : board -> int * int -> t -> board
(** Updates the board given an old board, move, and a piece*)
