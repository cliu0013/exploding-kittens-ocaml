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
              else if Deck.player_have_card e.game curr_id str then (
                let e =
                  { e with game = Deck.use_card e.game curr_id str }
                in
                let p = snd e.game in
                print_player2 p curr_id;
                turn_start e
                (* print invalid card name -- retry *))
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
    | BOMBED -> failwith "BOMBED unimplemented"
    | ATTACKED -> failwith "ATTACKED unimplemented"
    | DEAD -> failwith "DEAD unimplemented"

(* *)
(* let check_state = failwith "unimp" *)
