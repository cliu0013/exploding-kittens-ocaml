open OUnit2
open Deck
open Move

(* Test Plan: The functions in the Deck module which are responsible for
   representing a deck of cards for the game and a couple of basic
   functions on a deck were automatically tested by OUnit as they do not
   require manual user inputs in the terminal. The functions in the Move
   and main modules which run the game and handle game events and player
   interaction were tested manually as they require repeated manual user
   inputs in the terminal. For OUnit testing, test cases which aimed to
   test general functionality were developed with a black-box approach
   while test cases addressing edge cases were developed with a
   glass-box approach For manual testing, user inputs and the parameters
   for the game were developed with a black-box approach while
   randomization contained in CPU player moves for example helped find
   edge cases. Because the rules of Exploding Kittens are well defined
   and relatively straightforward, black-box testing based on mechanics
   of the game has been an effective testing strategy for determining if
   our game generally runs correctly. For edge cases and rare situations
   in game, the randomized moves that CPU players during much of our
   playtesting in addition to targeted glass-box testing allowed us to
   discover and address most of the remaining edge cases. *)

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

let original = from_json (Yojson.Basic.from_file "original.json")

let empty = from_json (Yojson.Basic.from_file "empty.json")

let card1 : card_id = { name = "Cattermelon"; genre = "kittens" }

let card2 : card_id = { name = "Hairy Potato Cat"; genre = "kittens" }

let card3 : card_id = { name = "Tacocat"; genre = "kittens" }

let safe_human_player = { hand = []; id = 0; state = SAFE }

let human_with_card = { hand = [ card1 ]; id = 0; state = SAFE }

let attacked_human_player = { hand = []; id = 0; state = ATTACKED }

let bombed_player = { hand = []; id = 1; state = BOMBED }

let safe_player = { hand = [ card1 ]; id = 2; state = SAFE }

let safe_ai_nocard = { hand = []; id = 2; state = SAFE }

let bombed_player2 = { hand = []; id = 2; state = BOMBED }

let attacked_player = { hand = []; id = 3; state = ATTACKED }

let dead_player = { hand = []; id = 4; state = DEAD }

let p1 =
  {
    ai = [ bombed_player; safe_player; attacked_player; dead_player ];
    user = safe_human_player;
  }

let p1_human_attacked =
  {
    ai = [ bombed_player; safe_player; attacked_player; dead_player ];
    user = attacked_human_player;
  }

let p1_player2_bombed =
  {
    ai = [ bombed_player; bombed_player2; attacked_player; dead_player ];
    user = safe_human_player;
  }

let p2 = { ai = [ dead_player ]; user = safe_human_player }

let no_ai_p = { ai = []; user = safe_human_player }

let p3 =
  {
    ai = [ bombed_player; safe_ai_nocard; attacked_player; dead_player ];
    user = human_with_card;
  }

let game1 = game_start original 1

let emptygame = game_start empty 1

let t1 : Deck.t = (original, p1)

let t2 : Deck.t = (original, p3)

let no_cards : Deck.t = (empty, p3)

let no_ai_t : Deck.t = (original, no_ai_p)

let deck_tests =
  [
    ( "num_cards on deck with positive number of cards" >:: fun _ ->
      assert_equal 56 (num_cards original) ~printer:string_of_int );
    ( "num_cards on deck with no cards" >:: fun _ ->
      assert_equal 0 (num_cards empty) ~printer:string_of_int );
    ( "num_alive with 3 alive players and 1 dead player" >:: fun _ ->
      assert_equal 3 (num_alive p1) ~printer:string_of_int );
    ( "num_alive with 0 alive players and 1 dead player" >:: fun _ ->
      assert_equal 0 (num_alive p2) ~printer:string_of_int );
    ( "shuffling empty list outputs empty list" >:: fun _ ->
      assert_equal [] (shuffle []) );
    ( "shuffling non empty list results in list of same length"
    >:: fun _ ->
      assert_equal
        (List.length [ card1; card2; card3 ])
        (List.length (shuffle [ card1; card2; card3 ]))
        ~printer:string_of_int );
    ( "get_hand of game with no cards outputs empty list" >:: fun _ ->
      assert_equal [] (get_hand emptygame) );
    ( "get_hand of game with cards outputs non empty list" >:: fun _ ->
      assert_equal
        [ { name = "Cattermelon"; genre = "kittens" } ]
        (get_hand t2) );
    ( "cards_left of game with no cards outputs empty list" >:: fun _ ->
      assert_equal [] (cards_left emptygame) );
    ( "cards_used of game with no cards outputs empty list" >:: fun _ ->
      assert_equal [] (cards_used emptygame) );
    ( "cards_info of game with no cards outputs empty list" >:: fun _ ->
      assert_equal [] (cards_info emptygame) );
    ( "mutating human player with correct id changes the human player"
    >:: fun _ ->
      assert_equal p1_human_attacked
        (mutate_p p1 attacked_human_player 0) );
    ( "mutating ai player with correct id changes the ai player"
    >:: fun _ ->
      assert_equal p1_player2_bombed (mutate_p p1 bombed_player2 2) );
    ( "mutating invalid player id does nothing" >:: fun _ ->
      assert_equal p1 (mutate_p p1 attacked_human_player 20) );
    ( "getting genre of valid card" >:: fun _ ->
      assert_equal "kittens" (get_genre t1 "Tacocat") );
    ( "getting genre of invalid card" >:: fun _ ->
      assert_equal "Cannot find card" (get_genre t1 "Mystery Card") );
    ( "finding human player" >:: fun _ ->
      assert_equal safe_human_player (find_player p1 0) );
    ( "finding ai player" >:: fun _ ->
      assert_equal safe_player (find_player p1 2) );
    ( "peek 0 cards from non-empty deck of cards " >:: fun _ ->
      assert_equal [] (peek t1 0) );
    ( "peek multiple cards from non-empty deck of cards " >:: fun _ ->
      assert_equal
        [
          { name = "Cattermelon"; genre = "kittens" };
          { name = "Cattermelon"; genre = "kittens" };
          { name = "Cattermelon"; genre = "kittens" };
        ]
        (peek t1 3) );
    ( "peek 0 cards from empty deck of cards " >:: fun _ ->
      assert_equal [] (peek emptygame 0) );
    ( "peek multiple cards from empty deck of cards " >:: fun _ ->
      assert_equal [] (peek emptygame 3) );
    ( "is_id of valid human player is true" >:: fun _ ->
      assert_equal true (is_id t1 0) );
    ( "is_id of valid ai player is true" >:: fun _ ->
      assert_equal true (is_id t1 1) );
    ( "is_id of invalid player is false" >:: fun _ ->
      assert_equal false (is_id t1 100) );
    ( "is_ai of valid human player in game with no ai is false"
    >:: fun _ -> assert_equal false (is_ai no_ai_t 0) );
    ( "is_ai of potentially valid player in game with no ai is false"
    >:: fun _ -> assert_equal false (is_ai no_ai_t 1) );
    ( "is_ai of invalid player in game with no ai is false " >:: fun _ ->
      assert_equal false (is_ai no_ai_t 100) );
    ( "is_ai of valid ai player is false" >:: fun _ ->
      assert_equal true (is_ai t1 1) );
    ( "is_ai of valid human player in game with ai players is false"
    >:: fun _ -> assert_equal false (is_ai t1 0) );
    ( "is_card of valid card is true" >:: fun _ ->
      assert_equal true (is_card t1 "Beard Cat") );
    ( "is_card of potentially valid card in game with no cards is false"
    >:: fun _ -> assert_equal false (is_card emptygame "Beard Cat") );
    ( "is_card of invalid card is false" >:: fun _ ->
      assert_equal false (is_card t1 "InvalidCard") );
    ( "have_card with human player that does not have the card is false"
    >:: fun _ -> assert_equal false (player_have_card t1 0 "Beard Cat")
    );
    ( "have_card with human player that does have the card is true"
    >:: fun _ -> assert_equal true (player_have_card t2 0 "Cattermelon")
    );
    ( "have_card with invalid player is false" >:: fun _ ->
      assert_equal false (player_have_card t1 100 "Cattermelon") );
    ( "have_card with ai player that does have the card is true"
    >:: fun _ -> assert_equal true (player_have_card t1 2 "Cattermelon")
    );
    ( "have_card with ai player that does not have the card is false"
    >:: fun _ ->
      assert_equal false (player_have_card t2 2 "Cattermelon") );
    ( "hand_is_empty is true for human player in empty game "
    >:: fun _ -> assert_equal true (hand_is_empty emptygame 0) );
    ( "hand_is_empty is true for ai player in empty game" >:: fun _ ->
      assert_equal true (hand_is_empty emptygame 1) );
    ( "hand_is_empty is false for ai player with card" >:: fun _ ->
      assert_equal false (hand_is_empty t1 2) );
    ( "hand_is_empty is true for ai player with empty hand" >:: fun _ ->
      assert_equal true (hand_is_empty t1 1) );
    ( "hand_is_empty is true for human player with empty hand"
    >:: fun _ -> assert_equal true (hand_is_empty t1 0) );
    ( "hand_is_empty is false for human player with card" >:: fun _ ->
      assert_equal false (hand_is_empty t2 0) );
    ( "transfer_card from ai player to human player" >:: fun _ ->
      assert_equal (t2, true) (transfer_card t1 2 0 "Cattermelon") );
    ( "transfer_card from human player to ai player" >:: fun _ ->
      assert_equal (t1, true) (transfer_card t2 0 2 "Cattermelon") );
    ( "num_copies of card is 0 for empty game" >:: fun _ ->
      assert_equal 0 (num_copies emptygame 0 "Cattermelon") );
    ( "num_copies of card is non-zero for ai player with card"
    >:: fun _ -> assert_equal 1 (num_copies t1 2 "Cattermelon") );
    ( "num_copies of card is zero for ai player without card"
    >:: fun _ -> assert_equal 0 (num_copies t1 2 "InvalidCard") );
    ( "num_copies of card is non-zero for human player with card"
    >:: fun _ -> assert_equal 1 (num_copies t2 0 "Cattermelon") );
    ( "num_copies of card is zero for human player without card"
    >:: fun _ -> assert_equal 0 (num_copies t2 0 "InvalidCard") );
    ( "check_state of human player" >:: fun _ ->
      assert_equal SAFE (check_state t1 0) );
    ( "check_state of ai player" >:: fun _ ->
      assert_equal ATTACKED (check_state t1 3) );
    ( "ai player with 1 card has empty hand after using 1 card"
    >:: fun _ ->
      assert_equal true
        (hand_is_empty (use_card t2 2 "Cattermelon" 1) 2) );
    ( "human player with 1 card has empty hand after using 1 card"
    >:: fun _ ->
      assert_equal true
        (hand_is_empty (use_card t2 0 "Cattermelon" 1) 0) );
    ( "human player with 1 card does not use any card after trying to \
       use card that they do not have"
    >:: fun _ ->
      assert_equal false (hand_is_empty (use_card t2 0 "Defuse" 1) 0) );
    ( "ai player with 1 card does not use any card after trying to use \
       card that they do not have"
    >:: fun _ ->
      assert_equal false (hand_is_empty (use_card t1 2 "Defuse" 1) 2) );
    ( "ai player with no cards does not have empty hand after drawing \
       card"
    >:: fun _ ->
      assert_equal false (hand_is_empty (fst (draw_card t1 3)) 3) );
    ( "human player with no cards does not have empty hand after \
       drawing card"
    >:: fun _ ->
      assert_equal false (hand_is_empty (fst (draw_card t1 0)) 0) );
    ( "no change after player attempts to draw card when there are no \
       cards available"
    >:: fun _ -> assert_equal no_cards (fst (draw_card no_cards 0)) );
  ]

let suite = "test suite" >::: List.flatten [ deck_tests ]

let _ = run_test_tt_main suite
