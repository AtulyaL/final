(* open Game open Pieces open Board open Logic *)

(* let game_over = raise (Failure "Unimplemented") let white_move = raise
   (Failure "Unimplemented") let black_move = raise (Failure "Unimplemented")
   let singleplayer = raise (Failure "Unimplemented") *)
let multiplayer =
  print_endline "Here is the current board: ";
  ()

let choose_gamemode =
  print_endline "Please choose a gamemode: singleplayer or multiplayer:\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  (* | "singleplayer" -> singleplayer *)
  | "multiplayer" -> multiplayer
  | _ -> raise (Failure "Please enter a valid gamemode")

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to our chess command line interface!\n";
  choose_gamemode

(* Execute the game engine. *)
let () = main ()
