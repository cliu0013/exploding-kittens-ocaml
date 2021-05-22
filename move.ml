open Deck

let debug = true

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

(* comparison function for names. returns 1 if first argument occurs
   earlier in the alphabet and 0 otherwise. *)
let compare_name (a : Deck.card_id) (b : Deck.card_id) : int =
  match a.name > b.name with true -> 1 | false -> 0

(* [print_player2] prints onto the command line the hand of the player
   with the specified [id]. *)
let print_player2 p id =
  if id = 0 then
    ANSITerminal.print_string [ ANSITerminal.green ]
      "\n********** Your current hand: **********\n"
  else if debug then
    ANSITerminal.print_string [ ANSITerminal.red ]
      ("\n ********** Player " ^ string_of_int id
     ^ "'s current hand (TEST ONLY): ********** \n")
  else ();
  if id = 0 || debug then (
    let player = Deck.find_player p id in
    let sorted = List.sort compare_name player.hand in
    print_endline
      (List.map
         (fun (h : Deck.card_id) -> h.name (*^ ": " ^ h.genre*))
         sorted
      |> String.concat "\n");
    print_endline "")

(* [turn_start] takes in an engine and returns an engine. Command line
   prompts are made to ask the user what move to make.*)
let rec turn_start e : e =
  let curr_id = e.curr_id in
  if curr_id <> 0 then manage_ai e
  else
    let curr_state = Deck.check_state e.game curr_id in
    match curr_state with
    | SAFE | ATTACKED ->
        let msg =
          "What card would you like to use? Type 'Pass' if you would \
           like to end your turn and draw a card.\n"
        in
        prompt_user e msg
    | BOMBED ->
        if Deck.player_have_card e.game curr_id "Defuse" then
          let rec prompt_defuse msg : e =
            print_endline msg;
            print_string "> ";
            match read_line () with
            | exception End_of_file -> failwith "read_line failure"
            | str ->
                if str = "Defuse" then
                  (* prompt for where to place bomb *)
                  let t = Deck.use_card e.game e.curr_id str 1 in
                  let t = Deck.change_state t e.curr_id SAFE in
                  let e = { e with game = t } in
                  end_turn e SAFE
                else prompt_defuse "That's not 'Defuse'!"
          in
          prompt_defuse "You drew a bomb! Quick, 'Defuse' it!"
        else (
          ANSITerminal.print_string [ ANSITerminal.green ]
            "\n********** Bombed! YOU LOSE. **********\n";
          e)
    | SKIPPED -> end_turn e SKIPPED
    | DEAD -> failwith "DEAD unimplemented"

(* [manage_spec_card] handles the logic for any valid card that the user
   wants to use. *)
and manage_spec_card (e : e) curr_id str =
  let t = e.game in
  (* let d = fst t in let p = snd t in *)
  (* let num = ref 1 in *)
  let genre = Deck.get_genre t str in
  match genre with
  | "kittens" -> manage_kittens e str
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
          let msg =
            "Attacked player " ^ (e.next_id |> string_of_int) ^ "!"
          in
          print_endline msg;
          (* set next player's state to ATTACKED *)
          let t = Deck.change_state e.game e.next_id ATTACKED in
          (* set user's state to SAFE (as per rules) *)
          let t = Deck.change_state t e.curr_id SAFE in
          (* immediately end turn *)
          let next_curr = (e.curr_id + 1) mod e.num_players in
          let next_next = (e.next_id + 1) mod e.num_players in
          let e = { e with game = t } in
          let e =
            {
              e with
              game = Deck.use_card t curr_id str 1;
              curr_id = next_curr;
              next_id = next_next;
            }
          in
          let p = snd e.game in
          print_player2 p curr_id;
          turn_start e
      | "See The Future" ->
          (* let top3 = Deck.peek e.game 3 in let count = ref 3 in while
             !count > 0 do let card = List.nth top3 !count in
             print_endline card.name; count := !count - 1 done; *)
          Deck.peek_print e.game 3;
          let e = { e with game = Deck.use_card t curr_id str 1 } in
          let p = snd e.game in
          print_player2 p curr_id;
          turn_start e
      | "Shuffle" ->
          (* shuffle the deck and update the engine with shuffled deck *)
          let pile = (fst e.game).cards_left in
          let pile = shuffle pile in
          let d = { (fst e.game) with cards_left = pile } in
          let t = (d, snd e.game) in
          let e = { e with game = t } in
          (* use the card *)
          let e = { e with game = Deck.use_card t curr_id str 1 } in
          let p = snd e.game in
          print_player2 p curr_id;
          turn_start e
      | "Favor" ->
          let msg =
            "Which player number would like to steal a random card \
             from?"
          in
          (* True if id is a valid player_id AND it is for an AI player *)
          let id_and_ai t num = Deck.is_id t num && num != 0 in
          let i = prompt_for_int_filter msg id_and_ai e.game in
          let curr_player = e.curr_id in
          let t, name = Deck.transfer_card_rand e.game curr_player i in
          print_endline ("You got a " ^ name ^ "!");
          let e = { e with game = t } in
          let e = { e with game = Deck.use_card t curr_id str 1 } in
          let p = snd e.game in
          print_player2 p curr_id;
          turn_start e
      | "Nope " ->
          (* TODO: nope *)
          let e = { e with game = Deck.use_card t curr_id str 1 } in
          let p = snd e.game in
          print_player2 p curr_id;
          turn_start e
      | _ -> failwith "Invalid card name")

(* [manage_kittens] prompts the user for a valid number of kittens to
   use and acts accordingly. 1/4: Nothing happens. 2/3: User gets to
   steal a random/named card from a named player. *)
and manage_kittens e str =
  let curr_id = e.curr_id in
  let t = e.game in
  let num_kittens = Deck.num_copies t curr_id str in
  (* print_endline msg; print_string "> "; *)
  let rec get_num_used msg =
    let i = prompt_for_int msg in
    if i <= num_kittens && i > 0 then
      match i with
      | 1 ->
          let msg =
            "You really can't do anything with one kitten. Please try \
             another card."
          in
          prompt_user e msg
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
          let msg = "4 kittens don't do anything. Please try again." in
          get_num_used msg
    else
      let msg = "Invalid number. Please try again." in
      get_num_used msg
  in
  let msg =
    "How many " ^ str ^ "s would you like to use? You have "
    ^ (num_kittens |> string_of_int)
    ^ "."
  in
  get_num_used msg

(*[multi_kittens] prompts to find the player that will be stolen from &
  does the stealing. **helper function for [manage_kittens] *)
and multi_kittens e (num : int) =
  match num with
  | 2 ->
      let msg =
        "Which player number would like to steal a random card from?"
      in
      let i = prompt_for_int_filter msg Deck.is_id e.game in
      let curr_player = e.curr_id in
      let t, name = Deck.transfer_card_rand e.game curr_player i in
      print_endline ("You got a " ^ name ^ "!");
      t
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

(* [prompt_for_int msg] outputs [msg] on the command line and checks
   that the user's response is an integer *)
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

(* [prompt_for_int msg filter t] outputs [msg] on the command line and
   checks that the user's response is an integer. It also checks if that
   integer is a valid given a specificed [filter] and [t]*)
and prompt_for_int_filter msg filter (t : t) =
  let value = prompt_for_int msg in
  if filter t value then value
  else
    let msg = "Not a valid number. Please try again." in
    prompt_for_int_filter msg filter t

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

(*from github:
  https://stackoverflow.com/questions/15095541/how-to-shuffle-list-in-on-in-ocaml *)
and shuffle d =
  let nd = List.map (fun c -> (Random.bits (), c)) d in
  let sond = List.sort compare nd in
  List.map snd sond

(* [end_turn e incr] takes care of the card draw that ends the turn and
   sets the player that goes next *)
and end_turn (e : e) (state : st) : e =
  let incr =
    match state with
    | SAFE | SKIPPED -> 1
    | ATTACKED -> 0
    | _ -> failwith "should not be in this state"
  in
  let next_curr = (e.curr_id + incr) mod e.num_players in
  let next_next = (e.next_id + incr) mod e.num_players in

  let t, is_bombed =
    if state != SKIPPED then (
      (* draw if not skipped *)
      let t, name = Deck.draw_card e.game e.curr_id in
      let e = { e with game = t } in
      if e.curr_id = 0 then print_endline ("You drew: " ^ name)
      else if debug then
        print_endline
          ("Player " ^ (e.curr_id |> string_of_int) ^ " drew: " ^ name);
      if name = "Exploding Kitten" then (t, true) else (t, false))
    else (e.game, false)
  in

  if is_bombed then (
    let e =
      {
        e with
        game = Deck.change_state e.game e.curr_id BOMBED;
        curr_id = next_curr;
        next_id = next_next;
      }
    in
    let p = snd e.game in
    print_player2 p e.curr_id;
    turn_start e)
  else
    (* move on *)
    let e = { e with curr_id = next_curr; next_id = next_next } in
    let p = snd e.game in
    print_player2 p e.curr_id;
    turn_start e

(* [prompt_user e msg] makes the command line prints and reads on the
   user's inputs. More specifically, it checks if the user's inputs are
   valid card names or keywords like 'Pass', then acts accordingly based
   on the game state.*)
and prompt_user e msg =
  let curr_state = Deck.check_state e.game e.curr_id in
  (* prompt user to enter a card name *)
  print_endline msg;
  print_string "> ";
  match read_line () with
  | exception End_of_file -> failwith "read_line failure"
  | str ->
      (* type "Pass" to draw a card and pass their turn *)
      if str = "Pass" then
        if curr_state = ATTACKED then
          (* ATTACKED -> SAFE & take another turn *)
          let t = Deck.change_state e.game e.curr_id SAFE in
          let e = { e with game = t } in
          end_turn e ATTACKED
        else (* move to next player *)
          end_turn e SAFE
      else if Deck.player_have_card e.game e.curr_id str then
        (* if the card is actually usable*)
        manage_spec_card e e.curr_id str
      else if Deck.is_card e.game str then (
        (* if the card exists but the player doesn't own it *)
        let msg =
          "You don't have that card! Please check your spelling, or \
           type 'Pass' to pass your turn.\n"
        in
        let p = snd e.game in
        print_player2 p e.curr_id;
        prompt_user e msg)
      else
        (* if the card just isn't in the game *)
        let msg =
          "That card doesn't exist! Please check your spelling, or \
           type 'Pass' to pass your turn.\n"
        in
        let p = snd e.game in
        print_player2 p 0;
        prompt_user e msg

(* [manage_ai] is the logic for ai players *)
and manage_ai e =
  let curr_id = e.curr_id in
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
    if curr_state = SKIPPED then Deck.change_state e.game e.curr_id SAFE
    else
      (* draw and set state to SAFE *)
      let temp, name = Deck.draw_card e.game curr_id in
      Deck.change_state temp e.curr_id SAFE
  in
  let e = { e with game; curr_id = next_curr; next_id = next_next } in
  let p = snd e.game in
  print_player2 p curr_id;
  turn_start e
