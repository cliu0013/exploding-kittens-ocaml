(** Represent a deck of cards for the Exploding Kittens game.

    This module represents the data stored in deck files, including a
    set of cards. It handles loading of that data from JSON as well as
    querying the data. *)

(** The abstract type of values representing the card deck. *)
type d

(** The type of card identifiers. *)
type card_id = string * int

type card_rem = string * int

(** [from_json j] is the card deck that [j] represents. Requires: [j] is
    a valid JSON representation for a deck of cards. *)
val from_json : Yojson.Basic.t -> d

(** [cards_start d] is a list of the card identifiers of the player's
    hand for the card deck [d]. *)
val cards_start : d -> card_id list

(** [cards_left d] is a list of all of cards that could be drawn later
    from the card deck [d]. *)
val cards_left : d -> card_id list

(** [cards_used d] is a list of all of cards that has been used and
    cannot be drawn from the card deck [d]. *)
val cards_used : d -> card_id list

(** [cards_info d] is a list of tuples where each tuple pairs a type of
    card with the number of that type of cards left in the deck [d]. *)
val cards_info : d -> card_rem list
