type name =
  | Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King

type color =
  | Black
  | White

type t
(** Type t is the abstract type that represents a piece. A piece should have a
    name, value, current location, information on whether it's captured, a list
    of possible moves, color, and whether it has moved or not *)

val init : name -> color -> int * int -> t
(** init initializes a piece given its name, color, and location *)

exception Color

val update_location : t -> int * int -> t
(** updates the location of the piece after making a move*)

val location : t -> int * int
(** returns the current location of the piece*)

val name : t -> name
(**returns name*)

val to_string : t -> string
val moved : t -> bool
val color : t -> color
val check_color : t -> color -> unit
val color_to_string : color -> string
