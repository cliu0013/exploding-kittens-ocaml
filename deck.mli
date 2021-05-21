(** Represents the deck of cards and the players for the Exploding
    Kittens game. *)

(** AF: [st] the states for the players. [BOMBED] means the player just
    drew an exploding kitten. [SAFE] means the player is safe.
    [ATTACKED] means the player is attacked by the other player. [DEAD]
    means the player is out of the game. *)
type st =
  | BOMBED
  | SAFE
  | ATTACKED
  | DEAD

type card_name = string

type card_id = {
  name : card_name;
  genre : string;
}

(** AF: [card_rem] the remainly cards in the deck. [copies] records the
    number of the card with name equal to [name] that are in the
    [cards_left] field of a deck [d]. ** RI: [copies] is non-negative. *)
type card_rem = {
  name : card_name;
  genre : string;
  copies : int;
}

(** AF: [d] the deck that represents the whole deck of cards, including
    [cards_used] which are all the cards that have been used,
    [cards_left] which are all the cards that can be drawn, and
    [cards_info] which records the information of the cards left (for
    the ease of testing). ** RI: [cards_left] is never empty because the
    bombs would never be drawn. [cards_info] is set-like. *)
type d = {
  cards_used : card_id list;
  cards_left : card_id list;
  cards_info : card_rem list;
  directory : card_id list;
}

type player_id = int

(** AF: [player] the player type that represents a single player. If
    [id] = 0 it implies that it's the human player. Otherwise, it's an
    AI player. ** RI: [id] is non-negative and unique. *)
type player = {
  hand : card_id list;
  id : player_id;
  state : st;
}

(** AF: [p] the players that represents all players of the game. ** RI:
    [user.id] = 0 and for any player [ply] in [ai] we have [ply.id] not
    equal to 0. Also [ai] is non-empty. *)
type p = {
  ai : player list;
  user : player;
}

(** AF: [t] the whole deck of cards and all the players. First entry is
    the deck, and the second entry is the players. *)
type t = d * p

(** [from_json j] is the card deck that [j] represents. RI: [j] is a
    valid JSON representation for a deck of cards. *)
val from_json : Yojson.Basic.t -> d

(* [num_cards d] is the number of cards in [d.cards_left] *)
val num_cards : d -> int

(* [num_alive p] is the number of AI players with state not equal to
   [DEAD] *)
val num_alive : p -> int

(* [shuffle lst] shuffle the list [lst] *)
val shuffle : card_id list -> card_id list

(** [cards_start d k] is the initialized game. The rule of dealing the
    start hand for the human player is to first exclude all Bombs and
    Diffuses from the deck, deal 7 random cards with 1 Diffuse to the
    person, and then put the removed Bombs and Diffuses back to the deck
    with a shuffle. [t = (d, p)] would be the return value where there
    will be [k] number of players in [p] including the human user. *)
val game_start : d -> int -> t

(** [get_hand t] is a list of all of cards that are in the hand of the
    human user. *)
val get_hand : t -> card_id list

(** [cards_left t] is a list of all of cards that can be drawn in later
    game. *)
val cards_left : t -> card_id list

(** [cards_left t] is a list of all of cards that have been used in the
    game. *)
val cards_used : t -> card_id list

(** [cards_info t] is a list of all the information of the cards that
    can be drawn in later game. *)
val cards_info : t -> card_rem list

(** [change_state t player_id st] changes the state of the player whose
    id is [player_id] to [st]. *)
val change_state : t -> player_id -> st -> t

(** [draw_card t player_id] let the player whose id is [player_id] to
    draw a card. The player try to draw the head of the deck's
    [cards_left]. If the player tries to draw a bomb, they don't draw it
    but instead change their state to [BOMBED]; otherwise draw the
    non-bomb card. *)
val draw_card : t -> player_id -> t

(** [rec use_card t player_id card_name num] lets the player whose id is
    [player_id] to use a card. The function puts the used card to
    [cards_used] of the deck and does nothing else in particular.
    use_card repeats for the [num] times ** RI: the player has the card.
    Should use [player_have_card t player_id card_name] to guarantee
    this RI. *)
val use_card : t -> player_id -> card_name -> int -> t

(** [transfer_card t player_id1 player_id2 card_name] let player1 whose
    id is [player_id1] to transfer the card whose name is [card_name] to
    player2 whose id is [player_id2]. ** RI: player1 has the card.
    Should use [player_have_card t player_id1 card_name] to guarantee
    this RI. *)
val transfer_card : t -> player_id -> player_id -> card_name -> t

(** [transfer_card_rand t player_id1 player_id2 card_name] is the same
    as the non-rand version, except that a random card is transferred
    rather than a specified card*)
val transfer_card_rand : t -> player_id -> player_id -> t

(** [take_card t player_id card_name] let player whose id is [player_id]
    to take a card from the used cards of the deck. ** RI: the used
    cards in the deck has the card. Should use
    [used_have_card t card_name] to guarantee this RI. *)
val take_card : t -> player_id -> card_name -> t

(** [used_have_card t card_name] returns true if there is a card with
    [card_name] in the used cards of the deck; otherwise returns false. *)
val used_have_card : t -> card_name -> bool

(** [used_have_card t card_name] returns true if there is a card with
    [card_name] in the hand of the player with id equal to [player_id];
    otherwise returns false. *)
val player_have_card : t -> player_id -> card_name -> bool

(* [check_state] returns the state of the given player. States are of
   type st. *)
val check_state : t -> player_id -> st

val find_player : p -> player_id -> player

(* [get_genre] takes in a card name and returns the genre for that card *)
val get_genre : t -> string -> string

val num_copies : t -> player_id -> string -> int

(* [is_id] returns a T/F for if the provided player_id actually exists *)
val is_id : t -> player_id -> bool

(* [is_card] returns T/F based on if [name] is a valid card name *)
val is_card : t -> string -> bool

(* [is_kitten] returns true iff the card is one of the kittens *)
val is_kitten : t -> string -> bool
