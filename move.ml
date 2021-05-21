open Deck

(* hand -> hand *)
(* transfer_card *)
(* hand -> cards_used *)
(* use_card *)
(* cards_left -> hand *)
(* draw_card *)
(* cards_used -> hand *)
(* take_card *)

(** Represents the actions that player can make on their turn. *)

(* check player state *)
(* if safe: continue *)
(* if dead: skip player// end game if player is human*)
(* if attacked: *)

type game_state =
  | WIN
  | LOSS
  | CONTINUE

type e = {
  curr_id : Deck.player_id;
  next_id : Deck.player_id;
  num_alive : int;
  num_players : int;
  game : Deck.t;
  game_st : game_state;
}

(* # players alive, # players in game *)

(* Print: Enter a card name to be used *)
(* valid card name -> use_card() -> *)
(* attack -> end turn, set next player's status to attacked*)
(* *)

(* [engine_init] initializes the engine for the game *)
let engine_init t =
  {
    curr_id = 0;
    next_id = 1;
    num_alive = Deck.num_alive (snd t);
    num_players = Deck.num_alive (snd t) + 1;
    game = t;
    game_st = CONTINUE;
  }

let print_player p =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\n********** Your current hand: **********\n";
  print_endline
    (List.map
       (fun (h : Deck.card_id) -> h.name ^ ": " ^ h.genre)
       p.user.hand
    |> String.concat "\n");
  print_endline ""

(* [print_player2] prints onto the command line the hand of the player
   with the specified [id]. *)
let print_player2 p id =
  if id = 0 then
    ANSITerminal.print_string [ ANSITerminal.green ]
      "\n********** Your current hand: **********\n"
  else
    ANSITerminal.print_string [ ANSITerminal.red ]
      ("\n ********** Player " ^ string_of_int id
     ^ "'s current hand (TEST ONLY): ********** \n");
  let player = Deck.find_player p id in
  print_endline
    (List.map
       (fun (h : Deck.card_id) -> h.name ^ ": " ^ h.genre)
       player.hand
    |> String.concat "\n");
  print_endline ""

(* [turn_start] takes in an engine and returns an engine. Command line
   prompts are made to ask the user what move to make.*)
(* -if player is SAFE, allows them to use a card (prints onto command
   line)*)
(* -AI players instantly pass their turn and draw a card*)
let rec turn_start e =
  let curr_id = e.curr_id in
  if curr_id <> 0 then (
    let next_curr = (e.curr_id + 1) mod e.num_players in
    let next_next = (e.next_id + 1) mod e.num_players in
    let e =
      {
        e with
        game = Deck.draw_card e.game curr_id;
        curr_id = next_curr;
        next_id = next_next;
      }
    in
    let p = snd e.game in
    print_player2 p curr_id;
    turn_start e)
  else
    let curr_state = Deck.check_state e.game curr_id in
    match curr_state with
    | SAFE ->
        let rec prompt_user msg =
          (* prompt user to enter a card name *)
          print_endline msg;
          print_string "> ";
          match read_line () with
          | exception End_of_file -> failwith "read_line failure"
          | str ->
              (* type "Pass" to draw a card and pass their turn *)
              if str = "Pass" then (
                let next_curr = (e.curr_id + 1) mod e.num_players in
                let next_next = (e.next_id + 1) mod e.num_players in
                let e =
                  {
                    e with
                    game = Deck.draw_card e.game curr_id;
                    curr_id = next_curr;
                    next_id = next_next;
                  }
                in
                let p = snd e.game in
                print_player2 p curr_id;
                turn_start e
                (* try to find and use card*))
              else if Deck.player_have_card e.game curr_id str then
                (* if the card's genre is a kitten, ask for number of
                   kittens*)
                (* find genre -- match response to genre*)
                (* ask for # of kittens *)
                (* let num = 1 in let genre = Deck.get_genre e.game str
                   in *)
                (* if genre = "kittens" then num = kittens_to_use
                   curr_id; *)
                (* let num = manage_genre e.game genre in num |>
                   string_of_int |> print_endline; *)
                manage_spec_card e curr_id str prompt_user
                (* let e = { e with game = Deck.use_card e.game curr_id
                   str num } in let p = snd e.game in print_player2 p
                   curr_id; turn_start e *)
                (* print invalid card name -- retry *)
              else
                (* TODO: separate logic for cards that exist but are not
                   in hand *)
                let msg =
                  "That card doesn't exist! Please check your \
                   spelling, or type 'Pass' to pass your turn.\n"
                in
                let p = snd e.game in
                print_player p;
                prompt_user msg
        in
        let msg =
          "What card would you like to use? Type 'Pass' if you would \
           like to end your turn and draw a card.\n"
        in
        prompt_user msg
    | BOMBED ->
        ANSITerminal.print_string [ ANSITerminal.green ]
          "\n********** You are bombed and you lose XD **********\n";
        e
    | ATTACKED -> failwith "ATTACKED unimplemented"
    | DEAD -> failwith "DEAD unimplemented"

and manage_genre d genre : int =
  (* let genre : string = Deck.get_genre d str in *)
  match genre with "kittens" -> 2 | _ -> 1

and manage_genre2 d genre : int = failwith ""

and manage_spec_card (e : e) curr_id str prompt =
  let t = e.game in
  (* let d = fst t in let p = snd t in *)
  let num = ref 1 in
  let genre = Deck.get_genre t str in
  match genre with
  | "kittens" ->
      let num_kittens = Deck.num_copies t curr_id str in
      (* print_endline msg; print_string "> "; *)
      let rec get_num_used msg =
        (* print_endline msg; print_string "> "; match read_line () with
           | exception End_of_file -> failwith "read_line failure" | str
           -> ( match int_of_string_opt str with | Some i -> *)
        let i = prompt_for_int msg in
        if i <= num_kittens && i > 0 then
          match i with
          | 1 ->
              let msg =
                "You really can't do anything with one kitten. Please \
                 try another card."
              in
              prompt msg
          | 2 ->
              multi_kittens e.game 2;
              let e = { e with game = Deck.use_card t curr_id str i } in
              let p = snd e.game in
              print_player2 p curr_id;
              turn_start e
          | 3 ->
              let e = { e with game = Deck.use_card t curr_id str i } in
              let p = snd e.game in
              print_player2 p curr_id;
              turn_start e
          | _ ->
              let msg =
                "4 kittens don't do anything. Please try again."
              in
              get_num_used msg
        else
          let msg = "Invalid number. Please try again." in
          get_num_used msg
        (* | None -> let msg = "Invalid number. Please try again." in
           get_num_used msg) *)
      in
      let msg =
        "How many " ^ str ^ "s would you like to use? You have "
        ^ (num_kittens |> string_of_int)
        ^ "."
      in
      get_num_used msg
  | _ ->
      let e = { e with game = Deck.use_card t curr_id str !num } in
      let p = snd e.game in
      print_player2 p curr_id;
      turn_start e

(*[multi_kittens] takes care of the behavior for using 2,3, and 5
  kittens. *)
and multi_kittens t (num : int) =
  match num with
  | 2 -> (
      let msg =
        "Which player number would like to steal a random card from?"
      in
      let i = prompt_for_int msg in
      match i with
      | exception End_of_file -> failwith "read_line failure"
      | str ->
          (* if i |> Deck.is_id t then *)
          1)
  | 3 -> failwith "multi 3"
  | 5 -> failwith "multi 5"
  | _ -> failwith "multi_kittens"

and prompt_for_int msg =
  print_endline msg;
  print_string "> ";
  match read_line () with
  | exception End_of_file -> failwith "read_line failure"
  | str -> (
      match int_of_string_opt str with
      | Some i -> i
      | None ->
          let msg = "Not a number. Please try again." in
          prompt_for_int msg)

(* let e = { e with game = Deck.use_card t curr_id str num } in (* let p
   = snd e.game in *) print_player2 p curr_id; turn_start e *)

(* *)
(* let check_state = failwith "unimp" *)
