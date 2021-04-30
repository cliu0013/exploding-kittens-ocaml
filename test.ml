open OUnit2
open Deck

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let original = from_json (Yojson.Basic.from_file "original.json")

let empty = from_json (Yojson.Basic.from_file "empty.json")

let cardid : card_id = { name = "Cattermelon"; genre = "kittens" }

let human_player = { hand = []; id = 0; state = SAFE }

let bombed_player = { hand = []; id = 1; state = BOMBED }

let safe_player = { hand = []; id = 2; state = SAFE }

let attacked_player = { hand = []; id = 3; state = ATTACKED }

let dead_player = { hand = []; id = 4; state = DEAD }

let p1 =
  {
    ai = [ bombed_player; safe_player; attacked_player; dead_player ];
    user = human_player;
  }

let p2 = { ai = [ dead_player ]; user = human_player }

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
  ]

let suite = "test suite for A2" >::: List.flatten [ deck_tests ]

let _ = run_test_tt_main suite
