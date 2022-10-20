type object_phrase = {
  piece : string;
  move : int * int;
}

type command = Piece of object_phrase

exception Invalid

let parse_helper lst =
  match lst with
  | [] -> raise Invalid
  | [ u1; u2 ] -> (int_of_string u1, int_of_string u2)
  | _ -> raise Invalid

let parse str =
  let x = String.split_on_char ' ' str in
  let lst = List.filter (fun y -> y <> "") x in
  match lst with
  | [] -> raise Invalid
  | u1 :: u2 ->
      if
        u1 = "pawn" || u1 = "knight" || u1 = "rook" || u1 = "queen"
        || u1 = "king" || u1 = "bishop"
      then Piece { piece = u1; move = parse_helper u2 }
      else raise Invalid
