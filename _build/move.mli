(** Represents the game state and contains the REPL loop for the game. *)
open Deck

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

(** [engine_init] initializes the engine for the game *)
val engine_init : t -> e

(** [turn_start] handles the logic for what happens at the beginning of
    a turn. Command line prompts are issued, and cards are used when
    appropriate. [turn_start] calls [prompt_user] if it is the human
    player's turn and [manage_ai] otherwise.*)
val turn_start : e -> e

(** [prompt_user e msg] makes the command line prints and reads on the
    user's inputs. It checks if the user's inputs are valid card names
    or keywords like 'Pass' and acts based on the user's state.*)
val prompt_user : e -> string -> e

(** [manage_ai] is the logic for ai players *)
val manage_ai : e -> e

(** [print_player p id] prints the hand of player [id]*)
val print_player : p -> int -> unit

(** [use_spec_card e card_name] uses the card specified by [card_name] *)
val use_spec_card : e -> card_name -> e

(** [use_kittens e card_name] is called when a user uses a kitten, e.g.:
    Tacocat. Command line prompts are made to get a valid number of
    kittens to use and the appropriate action is applied.*)
val use_kittens : e -> card_name -> e

(** [handle_transfer e num] is a helper function for [use_kittens] and
    carries out whatever action is appropriate given the usage of [num]
    kittens. 2: random card stolen from a to-be-asked-for player. 3:
    specific card stolen*)
val handle_transfer : e -> int -> t

(** [prompt_for_int msg] outputs [msg] on the command line and checks
    that the user's response is an integer. If the user does not enter
    an integer, they are asked to do so again. *)
val prompt_for_int : string -> int

(** [prompt_for_int msg filter t] outputs [msg] on the command line and
    checks that the user's response is an integer. It also checks if
    that integer is a valid given a specificed [filter] and [t]. If the
    user does not enter a valid integer, they are asked to do so again.*)
val prompt_for_int_filter :
  string -> (Deck.t -> Deck.player_id -> bool) -> Deck.t -> int

(** [prompt_for_name t msg] outputs [msg] on the command line and checks
    that the response is an actual card in the given [t]. *)
val prompt_for_name : Deck.t -> string -> string

(** [end_turn e st] takes care of the card draw that ends the turn and
    sets the player that goes next *)
val end_turn : e -> st -> e

(** [noped_append e i] adds player [i] to the noped list in [e] *)
val noped_append : e -> player_id -> e

(** [noped_remove e i] removes player [i] to the noped list in [e] *)
val noped_remove : e -> player_id -> e

(** [manage_bombed] is called whenever a user draws a bomb. If they have
    a defuse, they are prompted to use it. Otherwise, they lose the
    game.*)
val manage_bombed : e -> e

(** [prompt_attack] prompts the Attack card for the current player.
    Their turn is immediately ended, and if they were ATTACKED before,
    they are SAFE now. The next player's state is changed to ATTACKED. *)
val prompt_attack : e -> e

(** [use_attack e ] uses the Nope card for the current player.*)
val use_attack : e -> e

(** [prompt_nope] prompts the Nope card for the current player. The
    player is prompted to provide a non-human player id to be added to
    the Noped list. Players on the Noped list have their next non-Defuse
    action cancelled. *)
val prompt_nope : e -> e

(** [use_nope e i] uses the Nope card for the current player on player
    [i]*)
val use_nope : e -> player_id -> e

(** [prompt_favor] prompts the Favor card for the current player.The
    player is prompted to provide a non-human player id to steal a
    random card from and then steals from that player. If the player's
    hand is empty, another player id must be specified. *)
val prompt_favor : e -> e

(** [use_favor e i] uses the Favor card for the current player on player
    [i]*)
val use_favor : e -> player_id -> e

(** [use_future] uses the Future card for the current player. The top
    three cards' names are printed onto the command line. *)
val use_future : e -> e
