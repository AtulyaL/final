type t = {
  name : string; 
  value: int ; 
  loc : (int * int) ; 
  captured : bool; 
  moves : (int * int) list; 
  color : string;
  moved : bool
}
(** The type that represents the piece. [name] is the name of the piece. 
  [value] is the amount of points the piece is worth [loc] is the location of 
  the piece. [captured] is whether the piece is captured or not *)

module type PIECE = sig
val name : t -> string
(** Returns the name of the piece *)

val value : t -> int
(** Returns the value of the piece *)

val loc : t -> (int * int)
(** Returns the current location of the piece *)

val captured : t -> bool
(** Returns true or false based on if piece is on board or not*)

val moves : t -> (int * int) list 
(** Returns a list of the possible moves *)

val color : t -> string
(** Returns the color (black or white) of the piece *)

end

module Pawn : PIECE = struct
  let info = {name = "pawn"; value = 1; loc = (1, 1); captured = false; moves = []; color = "black"; moved = false}

  let name info = info.name

  let value info = info.value

  let loc info = info.loc

  let captured info = info.captured

  let moves info = info.moves

  let color info = info.color

end

module Bishop : PIECE = struct
  let info = {name = "bishop"; value= 3 ; loc = (1, 1) ; captured= false; moves = []; color = "black"; moved = false}
(** The type that represents the piece. [name] is the name of the piece. [value] is the amount of points the piece is worth
    [loc] is the location of the piece. [captured] is whether the piece is captured or not *)

    let name info = info.name

    let value info = info.value

    let loc info = info.loc

    let captured info = info.captured

    let moves info = info.moves
  
    let color info = info.color
end

module Knight : PIECE = struct
  let info = {name = "knight"; value= 3 ; loc = (1, 1) ; captured= false; moves = []; color = "black"; moved = false}
(** The type that represents the piece. [name] is the name of the piece. [value] is the amount of points the piece is worth
    [loc] is the location of the piece. [captured] is whether the piece is captured or not *)

    let name info = info.name

    let value info = info.value
    let loc info = info.loc
    let captured info = info.captured
    let moves info = info.moves
  
    let color info = info.color
end

module Rook : PIECE = struct
  let info = {name = "rook"; value = 5 ; loc = (1, 1) ; captured = false; moves = []; color = "black"; moved = false}
(** The type that represents the piece. [name] is the name of the piece. [value] is the amount of points the piece is worth
    [loc] is the location of the piece. [captured] is whether the piece is captured or not *)

    let name info = info.name

    let value info = info.value
    let loc info = info.loc
    let captured info = info.captured
    let moves info = info.moves
  
    let color info = info.color

end

module Queen : PIECE = struct
  let info = {name = "queen"; value= 9 ; loc = (1, 1) ; captured= false; moves = []; color = "black"; moved = false}
(** The type that represents the piece. [name] is the name of the piece. [value] is the amount of points the piece is worth
    [loc] is the location of the piece. [captured] is whether the piece is captured or not *)

    let name info = info.name

    let value info = info.value
    let loc info = info.loc
    let captured info = info.captured
    let moves info = info.moves
  
    let color info = info.color
end

module King : PIECE = struct
  let info = {name = "king"; value= int_of_float infinity ; loc = (1, 1) ; captured= false; moves = []; color = "black"; moved = false}
  let name info = info.name

  let value info = info.value
  let loc info = info.loc
  let captured info = info.captured
  let moves info = info.moves

  let color info = info.color
end