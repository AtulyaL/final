
exception Invalid of string
(** Raised when an illegal move is made. *) 

type command =
  | Go of string


type t = {name : string; value: int ; loc : (int*int) ; captured: bool}
(** The type that represents the piece. [name] is the name of the piece. [value] is the amount of points the piece is worth
    [loc] is the location of the piece. [captured] is whether the piece is captured or not *)

type moves = string list
(** The type [moves] is a string list that lists the valid types of moves the piece can make *)


 