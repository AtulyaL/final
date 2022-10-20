type t = {
  name : string;
  loc : int * int;
  (* captured : bool; *)
  (* moves : (int * int) list; *)
  color : string;
  moved : bool;
}

let init name color loc : t = { name; loc; color; moved = false }

let update_location (t : t) (new_loc : int * int) =
  { t with loc = new_loc; moved = true }

let location t = t.loc
let name t = t.name
let moved t = t.moved
let color t = t.color
(* let update_status t = { t with captured = true } *)
