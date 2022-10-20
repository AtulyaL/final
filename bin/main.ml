open Game
open Pieces
open Board
open Logic
open Command

(* let game_over = raise (Failure "Unimplemented") let white_move = raise
   (Failure "Unimplemented") let black_move = raise (Failure "Unimplemented")
   let singleplayer = raise (Failure "Unimplemented") *)
let print_board board = Board.to_string board

let rec white_move move board =
  try
    let new_move = Command.parse move in
    let new_board = update_board board new_move.move new_move.piece in
    black_move (update_board new_Board new_move.move new_move.piece)
  with Invalid -> (
    print_endline "Please enter a valid command";
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | move -> white_move move board)

and black_move move board = raise (Failure "Unimplemented")

let multiplayer =
  print_endline "Here is your current board. It is white's move.";
  print_board Board.init;
  print_endline
    {|Please type "print" any time you want to view the board again|};
  print_endline
    {|To make a move, type the name of the piece, the location of the piece, and 
    where you want to move it, such as "pawn 22 23"|};
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | move -> white_move move Board.init

let rec choose_gamemode gamemode =
  match gamemode with
  | "multiplayer" -> multiplayer
  | _ -> (
      print_endline "Please enter a valid file name!";
      print_string "> ";
      match read_line () with
      | exception End_of_file -> ()
      | gamemode -> choose_gamemode gamemode)

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to our chess command line interface!\n";
  print_endline "Please choose a gamemode: multiplayer.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | gamemode -> choose_gamemode gamemode

(* Execute the game engine. *)
let () = main ()
