type tile =
  | Empty of int * int
  | Full of int * int * Pieces.t

exception MissingPiece
(** The type of a singular tile of the board, which is either empty or a tuple
    of two integers *)

type board = tile list
(** The type of the board, which is represented a list of tiles *)

val empty_board : tile list ref -> unit
(** An empty board without any pieces*)

val init : board
(** Creates a starting board*)

val valid_move : board -> int * int -> Pieces.zcolor -> bool
(** Determines if a space is occupied or not based off of the given board and
    current tile*)

val update_board : board -> int * int -> Pieces.t -> board
(** Updates the board given an old board, move, and a piece *)

val find_piece : int * int -> board -> Pieces.t
(** find_piece finds the piece on the board given the location and particular
    board*)

val to_string : board -> string
(** Converts a board to a string for the command line*)

val to_lst : board -> string list list
(** Converts a board into a list of string lists which is printed to the
    terminal command interface*)

val is_empty : board -> int * int -> bool
(** States whether or not that position is empty*)

val isolate_black : board -> board
(** Converts a board to a board with only the black pieces*)

val isolate_white : tile list -> board
(** Converts a board to a board with only the white pieces*)