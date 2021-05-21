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
let rec turn_start e : e =
  let curr_id = e.curr_id in
  if curr_id <> 0 then (
    (* TODO: clean up state management with match rather than if
       statements *)
    let curr_state = Deck.check_state e.game curr_id in
    let next_curr =
      if curr_state != ATTACKED then (e.curr_id + 1) mod e.num_players
      else e.curr_id
    in
    let next_next =
      if curr_state != ATTACKED then (e.next_id + 1) mod e.num_players
      else e.next_id
    in
    let game =
      (* Don't draw if AI is skipped *)
      if curr_state = SKIPPED then
        Deck.change_state e.game e.curr_id SAFE
      else
        (* draw and set state to SAFE *)
        let temp = Deck.draw_card e.game curr_id in
        Deck.change_state temp e.curr_id SAFE
    in
    let e = { e with game; curr_id = next_curr; next_id = next_next } in
    let p = snd e.game in
    print_player2 p curr_id;
    turn_start e)
  else
    let curr_state = Deck.check_state e.game curr_id in
    match curr_state with
    | SAFE | ATTACKED ->
        let rec prompt_user msg =
          (* prompt user to enter a card name *)
          print_endline msg;
          print_string "> ";
          match read_line () with
          | exception End_of_file -> failwith "read_line failure"
          | str ->
              (* type "Pass" to draw a card and pass their turn *)
              if str = "Pass" (* set who goes next *) then (
                if curr_state = ATTACKED then (
                  (* ATTACKED -> SAFE & take another turn *)
                  let t = Deck.change_state e.game e.curr_id SAFE in
                  let e = { e with game = t } in
                  let e =
                    { e with game = Deck.draw_card e.game curr_id }
                  in
                  let p = snd e.game in
                  print_player2 p curr_id;
                  turn_start e)
                else
                  (* move to next player *)
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
              else if Deck.player_have_card e.game curr_id str then
                (* try to find and use card*)
                manage_spec_card e curr_id str prompt_user
              else if Deck.is_card e.game str then (
                (* if the card exists but the player doesn't own it *)
                let msg =
                  "You don't have that card! Please check your \
                   spelling, or type 'Pass' to pass your turn.\n"
                in
                let p = snd e.game in
                print_player p;
                prompt_user msg)
              else
                (* if the card just isn't in the game *)
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
        if Deck.player_have_card e.game curr_id "Defuse" then
          let rec manage_bomb msg : e =
            print_endline msg;
            print_string "> ";
            match read_line () with
            | exception End_of_file -> failwith "read_line failure"
            | str ->
                if str = "Defuse" then (
                  let t = Deck.change_state e.game e.curr_id SAFE in
                  let e = { e with game = t } in
                  let e =
                    { e with game = Deck.use_card t curr_id str 1 }
                  in
                  let p = snd e.game in
                  print_player2 p curr_id;
                  turn_start e)
                else manage_bomb "That's not 'Defuse'!"
          in
          manage_bomb "You drew a bomb! Quick, 'Defuse' it!"
        else (
          ANSITerminal.print_string [ ANSITerminal.green ]
            "\n********** Bombed! YOU LOSE. **********\n";
          e)
    | SKIPPED ->
        let next_curr = (e.curr_id + 1) mod e.num_players in
        let next_next = (e.next_id + 1) mod e.num_players in
        let e = { e with curr_id = next_curr; next_id = next_next } in
        let p = snd e.game in
        print_player2 p curr_id;
        turn_start e
    | DEAD -> failwith "DEAD unimplemented"

and manage_genre d genre : int =
  (* let genre : string = Deck.get_genre d str in *)
  match genre with "kittens" -> 2 | _ -> 1

and manage_genre2 d genre : int = failwith ""

and manage_spec_card (e : e) curr_id str prompt =
  let t = e.game in
  (* let d = fst t in let p = snd t in *)
  (* let num = ref 1 in *)
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
              let t = multi_kittens e 2 in
              let e = { e with game = t } in
              let e = { e with game = Deck.use_card t curr_id str i } in
              let p = snd e.game in
              print_player2 p curr_id;
              turn_start e
          | 3 ->
              let t = multi_kittens e 3 in
              let e = { e with game = t } in
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
  | _ -> (
      match str with
      | "Skip" ->
          let msg =
            "Skipped player " ^ (e.next_id |> string_of_int) ^ "!"
          in
          print_endline msg;
          let t = Deck.change_state e.game e.next_id SKIPPED in
          let e = { e with game = t } in
          let e = { e with game = Deck.use_card t curr_id str 1 } in
          let p = snd e.game in
          print_player2 p curr_id;
          turn_start e
      | "Attack" ->
          (* immediately end turn *)
          let msg =
            "Attacked player " ^ (e.next_id |> string_of_int) ^ "!"
          in
          print_endline msg;
          (* set next player's state to ATTACKED *)
          let t = Deck.change_state e.game e.next_id ATTACKED in
          (* set user's state to SAFE (as per rules) *)
          let t = Deck.change_state t e.curr_id SAFE in
          let e = { e with game = t } in
          let e = { e with game = Deck.use_card t curr_id str 1 } in
          let p = snd e.game in
          print_player2 p curr_id;
          turn_start e
      (* set next player's state to attacked *)
      (* if attacked, take another turn -- no need to mess with turn
         order*)
      | _ ->
          let e = { e with game = Deck.use_card t curr_id str 1 } in
          let p = snd e.game in
          print_player2 p curr_id;
          turn_start e)

(*[multi_kittens] takes care of the behavior for using 2,3, and 5
  kittens. *)
and multi_kittens e (num : int) =
  match num with
  | 2 ->
      let msg =
        "Which player number would like to steal a random card from?"
      in
      let i = prompt_for_int_filter msg Deck.is_id e.game in
      let curr_player = e.curr_id in
      Deck.transfer_card_rand e.game curr_player i
  | 3 ->
      let msg =
        "Which player number would like to steal a card from?"
      in
      let i = prompt_for_int_filter msg Deck.is_id e.game in
      let curr_player = e.curr_id in
      let msg = "What card would like to try to steal?" in
      let name = prompt_for_name e.game msg in
      let t, passed = Deck.transfer_card e.game curr_player i name in
      if passed then (
        print_endline ("You've recieved a " ^ name ^ "!");
        t)
      else (
        print_endline
          ("Congratulations. You played yourself. That player didn't \
            have a " ^ name ^ ".");
        t)
  | 5 -> failwith "multi 5"
  (* TODO: real game: player can revive a card if they use 5 unique
     cards in their turn -- not implementing atm *)
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

(* filter is a function that returns T/F based on input int *)
and prompt_for_int_filter msg filter (t : t) =
  let value = prompt_for_int msg in
  if filter t value then value else prompt_for_int_filter msg filter t

and prompt_for_name t msg : string =
  print_endline msg;
  print_string "> ";
  match read_line () with
  | exception End_of_file -> failwith "read_line failure"
  | str ->
      if Deck.is_card t str then str
      else
        let msg = "Not a valid card name. Please try again." in
        prompt_for_name t msg

(* let e = { e with game = Deck.use_card t curr_id str num } in (* let p
   = snd e.game in *) print_player2 p curr_id; turn_start e *)

(* *)
(* let check_state = failwith "unimp" *)
