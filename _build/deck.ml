(* Note: You may introduce new code anywhere in this file. *)

type card_id = {
  name : string;
  genre : string;
}

type card_rem = {
  name : string;
  genre : string;
  copies : int;
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

open Yojson.Basic.Util

let get_a_kind card_list new_kind =
  let name, n, genre =
    (new_kind.name, new_kind.copies, new_kind.genre)
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

let get_one_card_info c =
  {
    name = c |> member "name" |> to_string;
    genre = c |> member "genre" |> to_string;
    copies = c |> member "number" |> to_int;
  }

let get_cards_info json =
  json |> member "cards" |> to_list |> List.map get_one_card_info

(* from_json *)
let from_json json =
  let cards_info = json |> get_cards_info in
  let cards_left = cards_info |> get_rems_start [] in
  let d_of_json j =
    {
      cards_used : card_id list = [];
      cards_left : card_id list;
      cards_info : card_rem list;
    }
  in
  d_of_json json

(* failwith "unimplemented" *)

let rec get_defuse acc d player =
  match d.cards_left with
  | [] -> failwith "not possible"
  | h :: t ->
      if h.genre <> "defuse" then
        (* no update on the info yet for MS1 *)
        let d = { d with cards_left = t } in
        let acc = h :: acc in
        get_defuse acc d player
      else
        let d = { d with cards_left = acc @ t } in
        let player = { player with hand = [ h ] } in
        (d, player)

let rec draw_card d player n =
  let hand = player.hand in
  match n with
  | 0 -> (d, player)
  | n_l -> (
      match d.cards_left with
      | h :: t ->
          if h.genre <> "bomb" then
            (* no update on the info yet for MS1 *)
            let d = { d with cards_left = t } in
            let p = { player with hand = h :: hand } in
            draw_card d p (n_l - 1)
          else
            let d = { d with cards_left = t @ [ h ] } in
            draw_card d player n_l
      | _ -> failwith "Not possible")

(* shuffle
   (https://stackoverflow.com/questions/15095541/how-to-shuffle-list-in-on-in-ocaml) *)
let shuffle l =
  let nd = List.map (fun c -> (Random.bits (), c)) l in
  let sond = List.sort compare nd in
  List.map snd sond

let game_start d =
  let d = { d with cards_left = shuffle d.cards_left } in
  let tup = get_defuse [] d { hand = []; id = 0 } in
  let d = fst tup in
  let user = snd tup in
  draw_card d user 7

let cards_left d = d.cards_left

let cards_used d = d.cards_used

let cards_info d = d.cards_info

let draw_card d = failwith "unimplemented"

let peek d = match d.cards_left with [] -> empty_card | h :: t -> h

let pop d =
  (* match d.cards_left with | [] -> empty_card | h :: t -> (* update d
     to not have the card *) d.cards_left h *)
  failwith "unimplemented"

(* what happens to old d? *)
let append d = failwith "unimplemented"
