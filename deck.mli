(** Represent a deck of cards for the Exploding Kittens game.

    This module represents the data stored in deck files, including a
    set of cards. It handles loading of that data from JSON as well as
    querying the data. *)

(** The abstract type of values representing the card deck. *)

(** The type of card identifiers represented using three-entry tuples.
    The first entry is the genre of the card, the second entry is the
    kind/name for the card, the third entry is the initial number of
    this kind of card in the deck. *)

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

(** The type of card information represented using two-entry tuples. The
    first entry is the kind/name for the card, the second entry is
    remaining number of this kind of card in the deck. *)
type card_rem = {
  name : card_name;
  genre : string;
  copies : int;
}

(** The type representing the card deck. *)
type d = {
  cards_used : card_id list;
  cards_left : card_id list;
  cards_info : card_rem list;
}

type player_id = int

type player = {
  hand : card_id list;
  id : player_id;
  state : st;
}

type p = {
  ai : player list;
  user : player;
}

type t = d * p

(** [from_json j] is the card deck that [j] represents. Requires: [j] is
    a valid JSON representation for a deck of cards. *)
val from_json : Yojson.Basic.t -> d

val num_cards : d -> int

val num_alive : p -> int

val shuffle : card_id list -> card_id list

(** [cards_start d] is a list of the card identifiers of the player's
    hand for the card deck [d]. The rule of dealing the start hand for
    the human player is to first exclude all Bombs and Diffuses from the
    deck, deal 7 random cards with 1 Diffuse to the person, and then put
    the removed Bombs and Diffuses back to the deck with a shuffle. *)
val game_start : d -> int -> t

(** [cards_left d] is a list of all of cards that can be drawn from the
    card deck [d] later in the game. *)
val cards_left : t -> card_id list

(** [cards_used d] is a list of all of cards that has been used and
    cannot be drawn from the card deck [d] anymore. *)
val cards_used : t -> card_id list

(** [cards_info d] is a list of tuples where each tuple pairs a type of
    card with the number of that type of cards left in the deck [d]. *)
val cards_info : t -> card_rem list

val change_state : t -> player_id -> st -> t

val draw_card : t -> player_id -> t

val use_card : t -> player_id -> card_name -> t

val transfer_card : t -> player_id -> player_id -> card_name -> t

val take_card : t -> player_id -> card_name -> t

val player_have_card : t -> player_id -> card_name -> bool
