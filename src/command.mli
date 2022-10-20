type object_phrase = string list
type command = Piece of object_phrase

val parse : string -> command

exception Invalid
