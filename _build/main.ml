(** [play_game f] starts the adventure in file [f]. *)

let play_game f =
  try
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "\n\nValid file name.\n";
    print_endline
      "Please enter the number of AI players you want to play against\n";
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | str ->
        let n = int_of_string str in
        let d = Yojson.Basic.from_file f |> Deck.from_json in
        print_newline ();
        ANSITerminal.print_string [ ANSITerminal.green ]
          "\n********** Deck of the game: **********\n";
        print_endline
          (List.map
             (fun (h : Deck.card_rem) ->
               h.name ^ ", " ^ h.genre ^ ": " ^ string_of_int h.copies)
             d.cards_info
          |> String.concat "\n");
        print_endline
          ("Total number of cards: " ^ string_of_int (Deck.num_cards d));
        let d_hands_tuple = Deck.game_start d (n + 1) in
        let d = fst d_hands_tuple in
        let p = snd d_hands_tuple in
        ANSITerminal.print_string [ ANSITerminal.green ]
          "\n\
           ********** Your initial hand of cards (1 Defuse and 7 \
           others): **********\n";
        print_endline
          (List.map
             (fun (h : Deck.card_id) -> h.name ^ ": " ^ h.genre)
             p.user.hand
          |> String.concat "\n");
        print_endline "";
        print_endline
          ("Number of AI players remained: "
          ^ string_of_int (List.length p.ai));
        print_endline
          ("Total number of cards left: "
          ^ string_of_int (Deck.num_cards d));
        ANSITerminal.print_string [ ANSITerminal.red ]
          "\n********** Cards Info (TEST ONLY): **********\n";
        print_endline
          (List.map
             (fun (h : Deck.card_rem) ->
               h.name ^ ", " ^ h.genre ^ ": " ^ string_of_int h.copies)
             d.cards_info
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
  print_endline
    "Please enter the name of the game file you want to load\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

(* Execute the game engine. *)
let () = main ()
