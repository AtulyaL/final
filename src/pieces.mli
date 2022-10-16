type t
(** 
Type t is the abstract type that represents a piece. A piece should have a name,
value, current location, information on whether it's captured, a list of possible moves,
color, and whether it has moved or not 
    *)

val init_val : t
(** init is the intial state of the piece *)

val move : t -> (int*int) -> t
(** move is the state of the piece after moving to the given valid location *)

