type status
(** type status is a record containing the status of the game; its fields are
    "white", "black", and "turn", which is whether or not each side is in check
    and whose turn it is*)

val check : Board.board -> bool
(**check takes in a board and returns whether or not any side is in check*)

val checkmate : Board.board -> bool
(**checkmate takes in a board and returns whether or not any side is in
   checkmate*)

val update_status : status -> status
(**update_status takes in the status of the game and returns the new status of
   the game*)
