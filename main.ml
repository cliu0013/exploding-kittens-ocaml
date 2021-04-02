(** [play_game f] starts the adventure in file [f]. *)

open Deck

let play_game f =
  try
    let d = Yojson.Basic.from_file f |> Deck.from_json in
    print_newline ();
    let d_hand_tuple = Deck.game_start d in
    let d = fst d_hand_tuple in
    let p = snd d_hand_tuple in
    ANSITerminal.print_string [ ANSITerminal.green ]
      "\n\
       ********** Your initial hand of cards (1 Defuse and 7 others): \
       **********\n";
    print_endline
      (List.map
         (fun (h : Deck.card_id) -> h.name ^ ": " ^ h.genre)
         p.hand
      |> String.concat "\n")
  with e ->
    ANSITerminal.print_string [ ANSITerminal.red ]
      "Something went wrong (e.g. File Not Found). execute < make play \
       > in your terminal to try again. \n";
    exit 0

(** [main ()] prompts for the game to play, then starts it. *)

let main () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\nWelcome to the original Exploding Kittens Kit.\n";
  (* print_endline "Please enter the name of the game file you want to
     load.\n"; *)
  (* print_string "> "; *)
  (* match read_line () with | exception End_of_file -> () | file_name
     -> play_game file_name *)
  play_game "original.json"

(* Execute the game engine. *)
let () = main ()
