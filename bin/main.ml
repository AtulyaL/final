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
  p1color : Pieces.zcolor;
  p2color : Pieces.zcolor;
}

exception GameOver of outcome * name

let rec print_helper = function
  | [] -> print_string ""
  | u1 :: u2 ->
      print_string (u1 ^ " ");
      print_helper u2

let rec print_board i = function
  | [] ->
      print_endline "";
      print_endline ("\027[34m  1 2 3 4 5 6 7 8" ^ "\027[0m");
      print_endline ""
  | u1 :: u2 ->
      print_endline "";
      print_string ("\027[34m" ^ string_of_int i ^ " " ^ "\027[0m");
      print_helper u1;
      print_board (i - 1) u2

let print board = print_board 8 (to_lst board)

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
        else raise Invalid
  with Invalid -> (
    print_endline "Please enter a valid command.";
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

and check_msg names (color : Pieces.zcolor) =
  if color = Black then
    match names.p1color with
    | White -> Printf.printf "%s, you are in check" names.p2
    | Black -> Printf.printf "%s, you are in check" names.p1
  else
    match names.p1color with
    | White -> Printf.printf "%s, you are in check" names.p1
    | Black -> Printf.printf "%s, you are in check" names.p2

and process_helper board names (color : Pieces.zcolor) =
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "quit" ->
      print_endline "Goodbye! Thanks for playing!";
      exit 0
  | "forfeit" ->
      if color = White then game_over Black names else game_over White names
  | "stalemate" -> game_over Draw names
  | "board" ->
      print board;
      process_helper board names color
  | move -> process move board names color

and castle_helper d king rook move board names =
  let king_board = update_board board move king in
  match (d, move) with
  | Left, (t1, t2) ->
      let new_board = update_board king_board (t1, t2 - 4) rook in
      print new_board;
      if color king = White then
        proc_move new_board names (Black : Pieces.zcolor)
      else proc_move new_board names White
  | Right, (t1, t2) ->
      let new_board = update_board king_board (t1, t2 + 3) rook in
      print new_board;
      if color king = White then
        proc_move new_board names (Black : Pieces.zcolor)
      else proc_move new_board names White

and process move board names color : unit =
  try
    let new_move = parse move board in
    let new_board =
      update_board board (Command.move new_move) (piece new_move)
    in
    if check new_board color then raise Invalid
    else if check_move new_move.move new_move.piece board color then (
      print new_board;
      if color = White then proc_move new_board names (Black : Pieces.zcolor)
      else proc_move new_board names White)
    else raise Invalid
  with
  | Invalid ->
      print_endline "Please enter a valid command.";
      process_helper board names color
  | MissingPiece ->
      print_endline "There is no piece in the first set of coordinates";
      print_endline
        "Double check your coordinates; remember to start with row first then \
         column";
      process_helper board names color
  | Castle (d, king, rook) ->
      castle_helper d king rook (Command.move (parse move board)) board names

and proc_move board names (color : Pieces.zcolor) : unit =
  try
    if check_mate board color then
      if color = White then raise (GameOver (Black, names))
      else raise (GameOver (White, names));
    if stalemate board then raise (GameOver (Draw, names));
    if check board color then check_msg names color;
    if names.p1color = color then Printf.printf "It's %s's turn." names.p1
    else Printf.printf "It's %s's turn." names.p2;
    print_endline "";
    process_helper board names color
  with GameOver (o, n) -> game_over o n

and multiplayer board names =
  Printf.printf "Here is your current board. It is %s's move." names.p1;
  print_endline "";
  print board;
  print_endline
    {|To make a move, type the location of the piece and where you want to move it, with row first then column, such as "2 2 3 2"|};
  print_endline {|To quit during any part of the game, type "quit".|};
  print_endline
    {|The game will automatically assume stalemate when there are only two kings left, but if at any point before that there is a draw please type "stalemate" to end the game.|};
  print_endline
    {|If you would like to resign, type "forfeit" during your turn.|};
  print_endline {|To see the board again, type "board".|};
  process_helper board names White

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\n\
     Welcome to our chess command line interface!\n\
    \ This is a multiplayer game.";
  print_endline " ";
  print_endline " ";
  print_endline "Please enter player one's name (they will start as white).";
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
          p1color = White;
          p2color = Black;
        }
      in
      print_endline "Please enter player two's name (they will start as black).";
      print_string "> ";
      match read_line () with
      | exception End_of_file -> ()
      | name -> multiplayer Board.init { names with p2 = name })

(* Execute the game engine. *)
let () = main ()
