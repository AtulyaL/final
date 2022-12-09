open Game
open Board
open Command
open Logic
open Pieces

type outcome =
  | Black
  | White
  | Draw

type name = {
  p1 : string;
  p2 : string;
  p1score : int;
  p2score : int;
  p1check : bool;
  p2check : bool;
  p1color : Pieces.color;
  p2color : Pieces.color;
}

exception GameOver of outcome * name

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

let rec game_calc outcome name =
  match outcome with
  | Black ->
      print_endline "Good game!";
      if name.p1color = Black then { name with p1score = name.p1score + 1 }
      else { name with p2score = name.p2score + 1 }
  | White ->
      print_endline "Good game!";
      if name.p1color = White then { name with p1score = name.p1score + 1 }
      else { name with p2score = name.p2score + 1 }
  | Draw ->
      print_endline "Good game!";
      { name with p1score = name.p1score + 1; p2score = name.p2score + 1 }

and game_over outcome name =
  let name = game_calc outcome name in
  Printf.printf "The score is now %s: %i pts; %s: %i pts." name.p1 name.p1score
    name.p2 name.p2score;
  try
    print_endline
      "To play again, type the name of player who wishes to play as white. If \
       you want to quit, you can just type quit";
    match read_line () with
    | exception End_of_file -> ()
    | "quit" ->
        print_endline "Goodbye! Thanks for playing!";
        exit 0
    | white ->
        if
          (white = name.p1 && name.p1color = White)
          || (white = name.p2 && name.p2color = White)
        then multiplayer Board.init name
        else if white = name.p1 && name.p1color = Black then
          multiplayer Board.init { name with p1color = White; p2color = Black }
        else if white = name.p2 && name.p2color = Black then
          multiplayer Board.init { name with p2color = White; p1color = Black }
        else raise Invalid
  with Invalid -> (
    print_endline "Please enter a valid command";
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | "quit" ->
        print_endline "Goodbye! Thanks for playing!";
        exit 0
    | white ->
        if
          (white = name.p1 && name.p1color = White)
          || (white = name.p2 && name.p2color = White)
        then multiplayer Board.init name
        else if white = name.p1 && name.p1color = Black then
          multiplayer Board.init { name with p1color = White; p2color = Black }
        else if white = name.p2 && name.p2color = Black then
          multiplayer Board.init { name with p2color = White; p1color = Black }
        else raise Invalid)

and white_move move board names =
  try
    let new_move = parse move board in
    if check_move new_move.move new_move.piece board White then (
      let new_board =
        update_board board (Command.move new_move) (piece new_move)
      in
      print new_board;
      if check_mate board White then raise (GameOver (White, names));
      if check board White then
        match names.p1color with
        | White -> Printf.printf "%s, you are in check" names.p1
        | Black -> (
            Printf.printf "%s, you are in check" names.p2;
            Printf.printf "%s, make a move" names.p1;
            print_string "> ";
            match read_line () with
            | exception End_of_file -> ()
            | "quit" ->
                print_endline "Goodbye! Thanks for playing!";
                exit 0
            | move -> black_move move new_board names))
    else raise Invalid
  with
  | Invalid -> (
      print_endline "Please enter a valid command";
      print_string "> ";
      match read_line () with
      | exception End_of_file -> ()
      | "quit" ->
          print_endline "Goodbye! Thanks for playing!";
          exit 0
      | move -> white_move move board names)
  | GameOver (o, n) -> game_over o n

and black_move move board names =
  try
    let new_move = parse move board in
    if check_move new_move.move new_move.piece board Black then (
      let new_board =
        update_board board (Command.move new_move) (piece new_move)
      in
      print new_board;
      if check_mate board Black then raise (GameOver (Black, names));
      if check board Black then
        match names.p1color with
        | White -> Printf.printf "%s, you are in check" names.p2
        | Black -> (
            Printf.printf "%s, you are in check" names.p1;
            Printf.printf "%s, make a move" names.p2;
            print_string "> ";
            match read_line () with
            | exception End_of_file -> ()
            | "quit" ->
                print_endline "Goodbye! Thanks for playing!";
                exit 0
            | move -> white_move move new_board names))
    else raise Invalid
  with
  | Invalid -> (
      print_endline "Please enter a valid command";
      print_string "> ";
      match read_line () with
      | exception End_of_file -> ()
      | "quit" ->
          print_endline "Goodbye! Thanks for playing!";
          exit 0
      | move -> white_move move board names)
  | GameOver (o, n) -> game_over o n

and multiplayer board names =
  Printf.printf "Here is your current board. It is %s move." names.p1;
  print board;
  print_endline
    {|To make a move, type the location of the piece and where you want to move it, with row first then column, such as "2 2 3 2"|};
  print_endline "To quit during any part of the game, type quit";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "quit" ->
      print_endline "Goodbye! Thanks for playing!";
      exit 0
  | move -> white_move move board names

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\n\
     Welcome to our chess command line interface!\n\
    \ This is a multiplayer game.";
  print_endline "Please enter player one's name (they will start as white).\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | name -> (
      let names =
        {
          p1 = name;
          p2 = "No name";
          p1score = 0;
          p2score = 0;
          p1check = false;
          p2check = false;
          p1color = White;
          p2color = Black;
        }
      in
      match read_line () with
      | exception End_of_file -> ()
      | name -> multiplayer Board.init { names with p2 = name })

(* Execute the game engine. *)
let () = main ()
