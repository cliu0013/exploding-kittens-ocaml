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
val engine_init : t -> e

(* [turn_start] takes in an engine *)
(* -checks player state *)
(* if player is safe, allows them to make a move *)
val turn_start : t -> e

(* fxn to check game status given an engine*)
val check_state : e -> game_state
