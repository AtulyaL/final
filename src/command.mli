type object_phrase = {
  piece : Pieces.t;
  move : int * int;
}

val parse : string -> Board.board -> object_phrase
val move : object_phrase -> int * int
val piece : object_phrase -> Pieces.t

exception Invalid
