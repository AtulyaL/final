type object_phrase = {
  piece : Pieces.t;
  move : int * int;
}

type command = Piece of object_phrase

val parse : string -> command
val move : command -> int * int
val piece : command -> Pieces.t

exception Invalid
