type t
(** Type t is the abstract type that represents a piece. A piece should have a
    name, value, current location, information on whether it's captured, a list
    of possible moves, color, and whether it has moved or not *)

val init : string -> string -> int * int -> t
(** init initializes a piece given its name, color, and location *)

val update_location : t -> int * int -> t
(** updates the location of the piece after making a move*)

val location : t -> int * int
(** returns the current location of the piece*)

val name : t -> string
(**returns name*)

val moved : t -> bool
val color : t -> string