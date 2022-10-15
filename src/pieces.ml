module type PIECES = sig
(** The chess pieces and their respective moves*)

type name = string
(** The type [name] is the name of the piece*)

type value = int
(** The type [value] represents the value of the piece*)

type loc = (int*int) 
(** The type [loc] represents the location of the piece*)
type captured =  bool
(** The type[captured] represents whether this piece is captured or not*)

type command =
  | Go of string

exception Invalid
(** Raised when the move is invalid*)

type moves = string list
(** The type [moves] is a string list that lists the valid types of moves the piece can make *)

end


module Pawn : PIECES = struct
let name = "Pawn"
let value = 1
let captured = false

exception Invalid



 
end
