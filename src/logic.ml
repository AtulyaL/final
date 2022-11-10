open Pieces
open Board

(* exception Invalid *)
(* note that pieces can not jump over other pieces, except for knights*)

(* first horizontal, then vertical for moves

   checks if pawn move is valid; returns a bool *)

(* type status = { (* white : bool; black : bool; *) turn : color } *)

(*so turn would be of type Color*)
(* let beg = { (* white = false; black = false; *) turn = Black } *)

let pawn_move (move : int * int) info board color =
  let loc = location info in
  if color = White then
    match (move, loc) with
    | (u1, u2), (t1, t2) ->
        if u2 = t2 then
          if u1 = t1 + 1 then valid_move board move color
          else if u1 = t1 + 2 then
            valid_move board move color && not (moved info)
          else false
        else if (u2 = t2 + 1 || u2 = t2 - 1) && u1 = t1 + 1 then
          valid_move board move color
        else false
  else
    match (move, loc) with
    | (u1, u2), (t1, t2) ->
        if u2 = t2 then
          if u1 = t1 - 1 then valid_move board move color
          else if u1 = t1 - 2 then
            valid_move board move color && not (moved info)
          else false
        else if (u2 = t2 + 1 || u2 = t2 - 1) && u1 = t1 - 1 then
          valid_move board move color
        else false

let knight_move move info board color =
  let loc = location info in
  match (move, loc) with
  | (u1, u2), (t1, t2) ->
      if u2 = t2 + 1 || u2 = t2 - 1 then
        if u1 = t1 + 2 || u1 = t1 - 2 then valid_move board move color
        else false
      else if u2 = t2 + 2 || u2 = t2 - 2 then
        if u1 = t1 + 1 || u1 = t1 - 1 then valid_move board move color
        else false
      else false

let rook_move move info board color =
  let loc = location info in
  match (move, loc) with
  | (u1, u2), (t1, t2) ->
      if u2 = t2 || u1 = t1 then valid_move board move color else false

let bishop_move move info board color =
  let loc = location info in
  match (move, loc) with
  | (u1, u2), (t1, t2) ->
      if t2 - u2 = t1 - u1 || t2 - u2 = u1 - t1 then valid_move board move color
      else false

let queen_move move info board color =
  let loc = location info in
  match (move, loc) with
  | (u1, u2), (t1, t2) ->
      if t2 - u2 = t1 - u1 || u1 = t1 || u2 = t2 then
        valid_move board move color
      else false

let king_move move info board color =
  let loc = location info in
  match (move, loc) with
  | (u1, u2), (t1, t2) ->
      if u1 = t1 then
        if u2 = t2 + 1 || u2 = t2 - 1 then valid_move board move color
        else false
      else if u2 = t2 then
        if u1 = t1 + 1 || u1 = t1 - 1 then valid_move board move color
        else false
      else if u2 = t2 + 1 || u2 = t2 - 1 then
        if u1 = t1 + 1 || u1 = t1 - 1 then valid_move board move color
        else false
      else false

let check_move move info board color =
  match name info with
  | Pawn -> pawn_move move info board color
  | Knight -> knight_move move info board color
  | Rook -> rook_move move info board color
  | Bishop -> bishop_move move info board color
  | Queen -> queen_move move info board color
  | King -> king_move move info board color

(* let check = raise (Failure "Unimplemented, Atulya are you doing any work?")

   let checkmate = raise (Failure "Unimplemented, Atulya are you doing any\n
   work?")

   let update_status = raise (Failure "Unimplemented, Atulya are you doing any\n
   work?") *)
(*let check_jumps*)
