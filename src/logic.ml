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

(*if u2 = t2 then if u1 < t1 loop from u1 to t1 and check that it's empty*)
let rook_move move info board color =
  let loc = location info in
  let output = ref (valid_move board move color) in
  match (move, loc) with
  | (u1, u2), (t1, t2) ->
      if u2 = t2 then
        if u1 < t1 then
          for x = u1 + 1 to t1 - 1 do
            if !output && is_empty board (x, u2) then output := true
            else output := false
          done
        else
          for x = t1 + 1 to u1 - 1 do
            if !output && is_empty board (x, u2) then output := true
            else output := false
          done
      else if u1 = t1 then
        if u2 < t2 then
          for x = u2 + 1 to t2 - 1 do
            if !output && is_empty board (u1, x) then output := true
            else output := false
          done
        else
          for x = t2 + 1 to u2 - 1 do
            if !output && is_empty board (u1, x) then output := true
            else output := false
          done
      else output := false;
      !output

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

let pawn_reachable pos tile =
  let loc = location tile in
  match (pos, loc) with
  | (u1, u2), (t1, t2) ->
      if u2 = t2 then
        if u1 = t1 + 1 then true
        else if u1 = t1 + 2 then true && not (moved tile)
        else false
      else if (u2 = t2 + 1 || u2 = t2 - 1) && u1 = t1 + 1 then true
      else false

let knight_reachable pos tile =
  let loc = location tile in
  match (pos, loc) with
  | (u1, u2), (t1, t2) ->
      if u2 = t2 + 1 || u2 = t2 - 1 then
        if u1 = t1 + 2 || u1 = t1 - 2 then true else false
      else if u2 = t2 + 2 || u2 = t2 - 2 then
        if u1 = t1 + 1 || u1 = t1 - 1 then true else false
      else false

let rook_reachable pos tile =
  let loc = location tile in
  match (pos, loc) with
  | (u1, u2), (t1, t2) -> if u2 = t2 || u1 = t1 then true else false

let bishop_reachable pos tile =
  let loc = location tile in
  match (pos, loc) with
  | (u1, u2), (t1, t2) ->
      if t2 - u2 = t1 - u1 || t2 - u2 = u1 - t1 then true else false

let queen_reachable pos tile =
  let loc = location tile in
  match (pos, loc) with
  | (u1, u2), (t1, t2) ->
      if t2 - u2 = t1 - u1 || u1 = t1 || u2 = t2 then true else false

let king_reachable pos tile =
  let loc = location tile in
  match (pos, loc) with
  | (u1, u2), (t1, t2) ->
      if u1 = t1 then if u2 = t2 + 1 || u2 = t2 - 1 then true else false
      else if u2 = t2 then if u1 = t1 + 1 || u1 = t1 - 1 then true else false
      else if u2 = t2 + 1 || u2 = t2 - 1 then
        if u1 = t1 + 1 || u1 = t1 - 1 then true else false
      else false

let find_king c ti =
  match name ti with
  | King -> if color ti = c then true else false
  | _ -> false

let check_check pos tile =
  match name tile with
  | Pawn -> pawn_reachable pos tile
  | Knight -> knight_reachable pos tile
  | Rook -> rook_reachable pos tile
  | Bishop -> bishop_reachable pos tile
  | Queen -> queen_reachable pos tile
  | King -> king_reachable pos tile

let rec check_helper board pos : bool =
  match board with
  | [] -> true
  | h :: t -> check_check pos h && check_helper t pos

let check board color =
  let king = List.find (find_king color) board in
  let king_position = location king in
  if color = Black then check_helper (isolate_black board) king_position
  else check_helper (isolate_white board) king_position

(*Psuedocode: store the position of the king in a variable named king_position.
  Then, using king_position as a parameter call

  let checkmate = raise (Failure "Unimplemented, Atulya are you doing any\n
  work?")

  let update_status = raise (Failure "Unimplemented, Atulya are you doing any\n
  work?") *)
(*let check_jumps*)
