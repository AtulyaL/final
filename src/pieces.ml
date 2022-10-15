module type PIECES = sig
(** The chess pieces and their respective moves*)

type name = string
(** The type [name] is the name of the piece*)

type value = int
(** The type [value] represents the value of the piece*)

type command =
  | Go of string

exception Invalid
(** Raised when the move is invalid*)

type moves = string list
(** The type [moves] is a string list that lists the valid types of moves the piece can make *)

end

