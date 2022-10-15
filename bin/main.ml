open Game
(** Import necessary modules*)

open Pieces
open Board

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to our chess command line interface!\n";
  print_endline "Please choose a color (white or black):\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "white" -> play_white
  | "black" -> play_black

(* Execute the game engine. *)
let () = main ()
