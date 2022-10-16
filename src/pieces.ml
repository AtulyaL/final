type t = {
  name : string;
  value : int;
  loc : int * int;
  captured : bool;
  moves : (int * int) list;
  color : string;
  moved : bool;
}

let init_val name =
  match name with
  | "pawn" -> 1
  | "knight" -> 3
  | "bishop" -> 3
  | "rook" -> 5
  | "queen" -> 9
  | "king" -> 99
  | _ -> int_of_float infinity

let init name color loc : t =
  {
    name;
    value = name |> init_val;
    loc;
    captured = false;
    color;
    moves = [];
    moved = false;
  }

let move (t : t) (new_loc : int * int) = { t with loc = new_loc; moved = true }
