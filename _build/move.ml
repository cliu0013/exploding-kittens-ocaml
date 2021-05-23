open Deck

let debug = true

let noped : player_id list = []

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
  noped : player_id list;
}

let engine_init t =
  {
    curr_id = 0;
    next_id = 1;
    num_alive = Deck.num_alive (snd t);
    num_players = Deck.num_alive (snd t) + 1;
    game = t;
    game_st = CONTINUE;
    noped = [];
  }

(* comparison function for names. returns 1 if first argument occurs
   earlier in the alphabet and 0 otherwise. *)
let compare_name (a : Deck.card_id) (b : Deck.card_id) : int =
  match a.name > b.name with true -> 1 | false -> 0

let print_player p id =
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

let rec turn_start e : e =
  let curr_id = e.curr_id in
  if curr_id <> 0 then manage_ai e
  else
    let curr_state = Deck.check_state e.game curr_id in
    match curr_state with
    | SAFE | ATTACKED ->
        (* normal turn *)
        let p = snd e.game in
        print_player p curr_id;
        let msg =
          "What card would you like to use? Type 'Pass' if you would \
           like to end your turn and draw a card.\n"
        in
        let nope_msg =
          if List.mem curr_id e.noped then
            "** You are noped, and the next non-Defuse card that you \
             play will do nothing!"
          else ""
        in
        prompt_user e (msg ^ nope_msg)
    | BOMBED -> manage_bombed e
    | SKIPPED | ATTACKER ->
        failwith "should never start a turn with SKIPPED or ATTACKER"
        (* end_turn e SKIPPED *)
    | DEAD -> failwith "DEAD unimplemented"

and use_spec_card (e : e) str =
  (* let d = fst t in let p = snd t in *)
  (* let num = ref 1 in *)
  let genre = Deck.get_genre e.game str in
  match genre with
  | "kittens" -> use_kittens e str
  | _ -> (
      match str with
      | "Skip" -> end_turn e SKIPPED
      | "Attack" -> prompt_attack e
      | "See The Future" -> use_future e
      | "Shuffle" -> use_shuffle e
      | "Favor" -> prompt_favor e
      | "Nope" -> prompt_nope e
      | "Defuse" ->
          prompt_user e "Can't use Defuse by itself! Try another card."
      | _ -> failwith "Invalid card name")

and use_kittens e str =
  let curr_id = e.curr_id in
  let t = e.game in
  let num_kittens = Deck.num_copies t curr_id str in
  (* print_endline msg; print_string "> "; *)
  let rec get_num_used msg =
    let f t i = i <= num_kittens && i > 0 in
    let i = prompt_for_int_filter msg f e.game in
    match i with
    | 1 ->
        let msg =
          "You really can't do anything with one kitten. Please try \
           another card."
        in
        prompt_user e msg
    | 2 ->
        let t = handle_transfer e 2 in
        let t = Deck.use_card t curr_id str 2 in
        let e = { e with game = t } in
        turn_start e
    | 3 ->
        let t = handle_transfer e 3 in
        let t = Deck.use_card t curr_id str 3 in
        let e = { e with game = t } in
        turn_start e
    | _ ->
        let msg = "4 kittens don't do anything. Please try again." in
        get_num_used msg
  in
  let msg =
    "How many " ^ str ^ "s would you like to use? You have "
    ^ (num_kittens |> string_of_int)
    ^ "."
  in
  get_num_used msg

and handle_transfer e (num : int) =
  match num with
  | 2 ->
      let rec handle_transfer_2 msg =
        let i = prompt_for_int_filter msg Deck.is_id e.game in
        if not (Deck.hand_is_empty e.game i) then (
          let t, name = Deck.transfer_card_rand e.game i e.curr_id in
          print_endline ("You got: " ^ name ^ "!");
          t)
        else
          handle_transfer_2
            "That player's hand is empty. Please try another player."
      in
      handle_transfer_2
        "Which player number would like to steal a random card from?"
  | 3 ->
      let msg =
        "Which player number would like to steal a card from?"
      in
      let i = prompt_for_int_filter msg Deck.is_id e.game in
      let msg = "What card would like to try to steal?" in
      let name = prompt_for_name e.game msg in
      let t, passed = Deck.transfer_card e.game i e.curr_id name in
      if passed then (
        print_endline ("You recieved a " ^ name ^ "!");
        t)
      else (
        print_endline
          ("Congratulations. You played yourself. That player didn't \
            have a " ^ name ^ ".");
        t)
  | 5 -> failwith "multi 5"
  (* TODO: real game: player can revive a card if they use 5 unique
     cards in their turn -- not implementing atm *)
  | _ -> failwith "handle_transfer"

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

and end_turn (e : e) (state : st) : e =
  let incr =
    match state with
    | SAFE | SKIPPED | ATTACKER | BOMBED -> 1
    | ATTACKED -> 0
    | _ -> failwith "should not be in this state"
  in
  let next_curr = (e.curr_id + incr) mod e.num_players in
  let next_next = (e.next_id + incr) mod e.num_players in

  let e, is_bombed =
    if state = ATTACKED || state = SAFE then (
      (* draw if safe/attacked *)
      let t, name = Deck.draw_card e.game e.curr_id in
      let e = { e with game = t } in
      if e.curr_id = 0 then print_endline ("You drew: " ^ name)
      else if debug then
        print_endline
          ("Player " ^ (e.curr_id |> string_of_int) ^ " drew: " ^ name);
      if name = "Exploding Kitten" then (e, true) else (e, false))
    else (* don't draw if was bombed/ SKIPPED/ attacking *)
      (e, false)
  in

  if is_bombed then
    let e =
      { e with game = Deck.change_state e.game e.curr_id BOMBED }
    in
    turn_start e
  else
    (* move on *)
    let e = { e with curr_id = next_curr; next_id = next_next } in
    turn_start e

and prompt_user e msg =
  let curr_state = Deck.check_state e.game e.curr_id in
  (* prompt user to enter a card name *)
  print_endline msg;
  print_string "> ";
  match read_line () with
  | exception End_of_file -> failwith "read_line failure"
  | str ->
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
        if List.mem e.curr_id e.noped then
          (* fizzle if Noped *)
          let e =
            { e with game = Deck.use_card e.game e.curr_id str 1 }
          in
          let e = noped_remove e e.curr_id in
          turn_start e
        else (* allow if not Noped*)
          use_spec_card e str
      else if Deck.is_card e.game str then
        (* if the card exists but the player doesn't own it *)
        let msg =
          "You don't have that card! Please check your spelling, or \
           type 'Pass' to pass your turn.\n"
        in
        prompt_user e msg
      else
        (* if the card just isn't in the game *)
        let msg =
          "That card doesn't exist! Please check your spelling, or \
           type 'Pass' to pass your turn.\n"
        in
        prompt_user e msg

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
      if debug then
        print_endline
          ("Player " ^ (e.curr_id |> string_of_int) ^ " drew: " ^ name)
      else
        print_endline
          ("Player " ^ (e.curr_id |> string_of_int) ^ " drew a card.");
      Deck.change_state temp e.curr_id SAFE
  in
  let e = { e with game; curr_id = next_curr; next_id = next_next } in
  let p = snd e.game in
  if debug then print_player p curr_id;
  turn_start e

and noped_append (e : e) (i : player_id) : e =
  let noped = e.noped @ [ i ] in
  { e with noped }

and noped_remove (e : e) (i : int) : e =
  let noped = List.filter (fun x -> x = i) e.noped in
  { e with noped }

and manage_bombed e =
  if Deck.player_have_card e.game e.curr_id "Defuse" then
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
            let msg =
              "Bomb has been defused! What index would you like to \
               place the bomb back in? Top of the deck is 0."
            in
            let f t v = v >= 0 in
            let i = prompt_for_int_filter msg f e.game in
            let t = Deck.place_bomb t i in
            end_turn { e with game = t } BOMBED
          else prompt_defuse "That's not 'Defuse'!"
    in
    prompt_defuse "Quick, 'Defuse' it!"
  else (
    ANSITerminal.print_string [ ANSITerminal.green ]
      "\n********** Bombed! YOU LOSE. **********\n";
    e)

and prompt_attack (e : e) : e =
  let msg = "Attacked player " ^ (e.next_id |> string_of_int) ^ "!" in
  print_endline msg;
  use_attack e

and use_attack e =
  (* set next player's state to ATTACKED *)
  let t = Deck.change_state e.game e.next_id ATTACKED in
  (* set user's state to SAFE (as per rules) *)
  let t = Deck.change_state t e.curr_id SAFE in
  (* use card and end turn *)
  let t = Deck.use_card t e.curr_id "Attack" 1 in
  let e = { e with game = t } in
  end_turn e ATTACKER

and use_shuffle (e : e) : e =
  let pile = (fst e.game).cards_left in
  let pile = Deck.shuffle pile in
  let d = { (fst e.game) with cards_left = pile } in
  let t = (d, snd e.game) in
  let t = Deck.use_card t e.curr_id "Shuffle" 1 in
  let e = { e with game = t } in
  turn_start e

and prompt_nope (e : e) : e =
  let msg = "Which player number would you like to 'Nope'?" in
  (* filter for if specified player is AI & not already Noped *)
  let f t i = Deck.is_ai t i && not (List.mem e.curr_id e.noped) in
  let i = prompt_for_int_filter msg f e.game in
  use_nope e i

and use_nope e i =
  let e = noped_append e i in
  let e = { e with game = Deck.use_card e.game e.curr_id "Nope" 1 } in
  turn_start e

and prompt_favor e =
  let rec handle_favor msg =
    (* True if id is a valid player_id AND it is for an AI player *)
    let i = prompt_for_int_filter msg Deck.is_ai e.game in
    if not (Deck.hand_is_empty e.game i) then use_favor e i
    else
      handle_favor
        "That player's hand is empty. Please try another player."
  in
  handle_favor
    "Which player number would like to steal a random card from?"

and use_favor e (giver : player_id) : e =
  let taker = e.curr_id in
  let t, name = Deck.transfer_card_rand e.game giver taker in
  if taker = 0 then print_endline ("You got: " ^ name ^ "!")
  else
    print_endline
      ("Player " ^ string_of_int taker ^ " got: " ^ name
     ^ " from player " ^ string_of_int giver ^ " !");
  let t = Deck.use_card t taker "Favor" 1 in
  let e = { e with game = t } in
  turn_start e

and use_future e : e =
  print_endline "The top three cards on the deck are: ";
  Deck.peek_print e.game 3;
  let e =
    { e with game = Deck.use_card e.game e.curr_id "See The Future" 1 }
  in
  turn_start e
