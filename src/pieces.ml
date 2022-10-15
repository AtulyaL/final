module type PIECE = sig

type t = {name : string; value: int ; loc : (string * int) ; captured: bool; moves: string list; color: string}
(** The type that represents the piece. [name] is the name of the piece. [value] is the amount of points the piece is worth
    [loc] is the location of the piece. [captured] is whether the piece is captured or not *)

type moves = string list
(** The type [moves] is a string list that lists the valid types of moves the piece can make *) 

val name
(** Returns the name of the piece*)

val value 
(** Returns the value of the piece*)
end

module Pawn : PIECE = struct

  let t = {name = "pawn"; value = 1; loc = (string, 1); captured = false}

  let moves = []
end


module Bishop : PIECE = struct
  let t = {name = ""}
end
 