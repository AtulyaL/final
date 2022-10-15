module type PIECE = sig

type t = {name : string; value: int ; loc : (char * int) ; captured: bool; moves: (int * int) list; color: string}
(** The type that represents the piece. [name] is the name of the piece. [value] is the amount of points the piece is worth
    [loc] is the location of the piece. [captured] is whether the piece is captured or not *)

val name : t -> string
(** Returns the name of the piece*)

val value : t -> int
(** Returns the value of the piece*)

val loc : t -> (int * int)
(** *)

val captured : t -> bool

val moves : t -> (int * int) list 

val color : t -> string


end


module Pawn : PIECE = struct
  let t = {name = "pawn"; value: 1 ; loc : ('A', 1) ; captured: false; moves: (int * int) list; color: string}
(** The type that represents the piece. [name] is the name of the piece. [value] is the amount of points the piece is worth
    [loc] is the location of the piece. [captured] is whether the piece is captured or not *)

  let moved = false
  let name = t.name
(** Returns the name of the piece*)

let value = t.value
(** Returns the value of the piece*)

let loc = t.loc
(** *)

let captured = t.captured

let moves = t.moves

let color = t.color


end

module Bishop : PIECE = struct
  let t = {name = "bishop"; value: 3 ; loc : ('A', 1) ; captured: false; moves: (int * int) list; color: string}
(** The type that represents the piece. [name] is the name of the piece. [value] is the amount of points the piece is worth
    [loc] is the location of the piece. [captured] is whether the piece is captured or not *)

  let name = t.name
(** Returns the name of the piece*)

let value = t.value
(** Returns the value of the piece*)

let loc = t.loc
(** *)

let captured = t.captured

let moves = t.moves

let color = t.color


end

module Knight : PIECE = struct
  let t = {name = "knight"; value: 3 ; loc : ('A', 1) ; captured: false; moves: (int * int) list; color: string}
(** The type that represents the piece. [name] is the name of the piece. [value] is the amount of points the piece is worth
    [loc] is the location of the piece. [captured] is whether the piece is captured or not *)

  let name = t.name
(** Returns the name of the piece*)

let value = t.value
(** Returns the value of the piece*)

let loc = t.loc
(** *)

let captured = t.captured

let moves = t.moves

let color = t.color


end

module Rook : PIECE = struct
  let t = {name = "rook"; value: 5 ; loc : ('A', 1) ; captured: false; moves: (int * int) list; color: string}
(** The type that represents the piece. [name] is the name of the piece. [value] is the amount of points the piece is worth
    [loc] is the location of the piece. [captured] is whether the piece is captured or not *)

  let name = t.name
(** Returns the name of the piece*)

let value = t.value
(** Returns the value of the piece*)

let loc = t.loc
(** *)

let captured = t.captured

let moves = t.moves

let color = t.color


end

module Queen : PIECE = struct
  let t = {name = "queen"; value: 9 ; loc : ('A', 1) ; captured: false; moves: (int * int) list; color: string}
(** The type that represents the piece. [name] is the name of the piece. [value] is the amount of points the piece is worth
    [loc] is the location of the piece. [captured] is whether the piece is captured or not *)

  let name = t.name
(** Returns the name of the piece*)

let value = t.value
(** Returns the value of the piece*)

let loc = t.loc
(** *)

let captured = t.captured

let moves = t.moves

let color = t.color


end

module King : PIECE = struct
  let t = {name = "king"; value: inf ; loc : ('A', 1) ; captured: false; moves: (int * int) list; color: string}
(** The type that represents the piece. [name] is the name of the piece. [value] is the amount of points the piece is worth
    [loc] is the location of the piece. [captured] is whether the piece is captured or not *)

  let name = t.name
(** Returns the name of the piece*)

let value = t.value
(** Returns the value of the piece*)

let loc = t.loc
(** *)

let captured = t.captured

let moves = t.moves

let color = t.color


end