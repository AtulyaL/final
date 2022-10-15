module type PIECE = sig

type t = {name : string; value: int ; loc : (char * int) ; captured: bool; moves: string list; color: string}
(** The type that represents the piece. [name] is the name of the piece. [value] is the amount of points the piece is worth
    [loc] is the location of the piece. [captured] is whether the piece is captured or not *)

val init : t
(** Returns a piece with all fields initialized*)
val name : t -> string
(** Returns the name of the piece*)

val value : t -> int
(** Returns the value of the piece*)

val loc : t -> (char * int)
(** *)

val captured : t -> bool

val moves : t -> string list 

val color : t -> string


end

x = Pawn
name = Pawn.name x.t

module Pawn : PIECE = struct

  let t = {name = "pawn"; value = 1; loc = (string, 1); captured = false}

  let moves = []
end


module Bishop : PIECE = struct
  let t = {name = ""}
end
 