open Pieces
open Board

(**first horizontal, then vertical for moves*)

(** checks if pawn move is valid; returns a bool*)

type status = {
  white : bool;
  black : bool;
  turn : string;
}

let pawn_move move info board color =
  let loc = location info in
  match (move, loc) with
  | (u1, u2), (t1, t2) ->
      if u1 = t1 then
        if u2 = t2 + 1 then valid_move board move color
        else if u2 = t2 + 2 then valid_move board move color && not (moved info)
        else false
      else if (u1 = t1 + 1 || u1 = t1 - 1) && u2 = t2 + 1 then
        valid_move board move color
      else false

(* let knight_move move info board = raise (Failure "Unimplemented") let
   rook_move move info board = raise (Failure "Unimplemented") let bishop_move
   move info board = raise (Failure "Unimplemented") let queen_move move info
   board = raise (Failure "Unimplemented") let king_move move info board = raise
   (Failure "Unimplemented") *)

let check = raise (Failure "Unimplemented, Atulya are you doing any work?")
let checkmate = raise (Failure "Unimplemented, Atulya are you doing any work?")

let update_status =
  raise (Failure "Unimplemented, Atulya are you doing any work?")

let check_move move info board color =
  match name info with
  | "pawn" -> pawn_move move info board color
  (* | "knight" -> knight_move move info board | "rook" -> rook_move move info
     board | "bishop" -> bishop_move move info board | "queen" -> queen_move
     move info board | "king" -> king_move move info board *)
  | _ -> raise (Failure "Unimplemented")
