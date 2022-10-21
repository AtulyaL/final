open Game
open Board
open Command

let rec print_helper = function
  | [] -> print_string ""
  | u1 :: u2 ->
      print_string (u1 ^ " ");
      print_helper u2

let rec print_board = function
  | [] -> print_endline ""
  | u1 :: u2 ->
      print_endline "";
      print_helper u1;
      print_board u2

let print board = print_board (to_lst board)

let rec white_move move board =
  try
    let new_move = parse move board in
    let new_board =
      update_board board (Command.move new_move) (piece new_move)
    in
    print new_board;
    match read_line () with
    | exception End_of_file -> ()
    | move -> black_move move new_board
  with Invalid -> (
    print_endline "Please enter a valid command";
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | "print" -> print board
    | move -> white_move move board)

and black_move move board =
  try
    print board;
    let new_move = parse move board in
    let new_board =
      update_board board (Command.move new_move) (piece new_move)
    in
    match read_line () with
    | exception End_of_file -> ()
    | move -> white_move move new_board
  with Invalid -> (
    print_endline "Please enter a valid command";
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | move -> black_move move board)

let multiplayer =
  print_endline "Here is your current board. It is white's move.";
  print Board.init;
  print_endline
    {|To make a move, type the location of the piece and where you want to move it, with row first then column, such as "2 2 3 2"|};
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | move -> white_move move init

let singleplayer =
  print_endline "not implemented yet lol";
  raise (Failure "Unimplemented")

let rec choose_gamemode gamemode =
  match gamemode with
  | "multiplayer" -> multiplayer
  | "singleplayer" -> singleplayer
  | _ -> (
      print_endline "Please enter a valid gamemode (multiplayer)!";
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
