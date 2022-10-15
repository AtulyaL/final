open Game
(** Import necessary modules*)

open Pieces
open Board

let singleplayer = raise (Failure "Unimplemented")
let multiplayer = raise (Failure "Unimplemented")

let rec choose_gamemode =
  print_endline "Please choose a gamemode: singleplayer or multiplayer:\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "singleplayer" -> singleplayer
  | "multiplayer" -> multiplayer
  | _ -> choose_gamemode

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to our chess command line interface!\n";
  choose_gamemode

(* Execute the game engine. *)
let () = main ()
