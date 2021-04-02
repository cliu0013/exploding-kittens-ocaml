(* Note: You may introduce new code anywhere in this file. *)

type card_id = {
  name : string;
  genre : string;
}

type card_rem = {
  name : string;
  copies : int;
  genre : string;
}

type d = {
  cards_used : card_id list;
  cards_left : card_id list;
  cards_info : card_rem list;
}

type player = {
  hand : card_id list;
  id : int;
}

type p = {
  ai : player list;
  user : player;
}

let empty_card = { name = ""; genre = "" }

(* TODO: replace [unit] with a type of your own design. *)
open Yojson.Basic.Util

let get_a_kind card_list new_kind =
  let name, genre, n =
    (new_kind.name, new_kind.genre, new_kind.copies)
  in
  let rec get_one_card new_list n_left =
    match n_left with
    | 0 -> new_list
    | n_l ->
        let card = { name; genre } in
        let new_list = card :: new_list in
        get_one_card new_list (n_l - 1)
  in
  card_list @ get_one_card [] n

let rec get_rems_start acc = function
  | [] -> acc
  | h :: r ->
      let acc = get_a_kind acc h in
      get_rems_start acc r

let from_json json =
  (* let parse_genres j = let kittens = let get_kittens v = { name = v
     |> member "name" |> to_string; genre = "kittens" } in let fxns =
     let get_fxns v = { name = v |> member "name" |> to_string; genre =
     "functionals"; } in j |> member "functionals" |> to_list |>
     List.map get_fxns in kittens @ fxns @ diffuses @ bombs in *)
  (* let get_genres c = { name = c |> member "name" |> to_string; genre
     = c |> member "genre" |> to_string; } in let parse_cards j = j |>
     member "cards" |> to_list |> List.map get_genres in *)
  let get_rems j =
    let get_numbers c =
      {
        name = c |> member "name" |> to_string;
        copies = c |> member "number" |> to_int;
        genre = c |> member "genre" |> to_string;
      }
    in
    j |> member "cards" |> to_list |> List.map get_numbers
  in
  let cards_info = get_rems json in
  let cards_left = get_rems_start [] cards_info in
  (* let rec parse_cards card_rems acc1= let rec parse_rec cards acc2 =
     match cards with | [] -> [] | h :: t -> (* h : card rem *)

     in parse_rec card_rems [] in parse_cards cards_info [] in *)
  let d_of_json j =
    {
      cards_used : card_id list = [];
      cards_left : card_id list;
      cards_info : card_rem list;
    }
  in
  let parse j : d =
    try d_of_json j
    with Type_error (s, _) -> failwith ("Parsing error: " ^ s)
  in
  parse json

(* failwith "unimplemented" *)

let cards_start d = failwith "unimplemented"

let cards_left d = d.cards_left

let cards_used d = d.cards_used

let cards_info d = d.cards_info

let draw_card d = failwith "unimplemented"

let peak d = match d.cards_left with [] -> empty_card | h :: t -> h

let pop d =
  (* match d.cards_left with | [] -> empty_card | h :: t -> (* update d
     to not have the card *) d.cards_left h *)
  failwith "unimplemented"

(* what happens to old d? *)
let append d = failwith "unimplemented"
