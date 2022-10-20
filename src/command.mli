type object_phrase = {
  piece : string;
  move : int * int;
}

type command = Piece of object_phrase

val parse : string -> command

exception Invalid
