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

(* [turn_start] takes in an engine *)
(* -checks player state *)
(* if player is safe, allows them to make a move (prints onto command
   line)*)
let turn_start e =
  let curr_id = e.curr_id in
  let curr_state = Deck.check_state e.game curr_id in
  match curr_state with
  | SAFE ->
      (* prompt user to enter a card name *)
      (* type "PASS" to draw a card and pass their turn *)
      { e with game = Deck.draw_card e.game curr_id }
      (* otherwise: use_card with input from user prompt*)
  | BOMBED -> failwith "BOMBED unimplemented"
  | ATTACKED -> failwith "ATTACKED unimplemented"
  | DEAD -> failwith "DEAD unimplemented"

(* *)
