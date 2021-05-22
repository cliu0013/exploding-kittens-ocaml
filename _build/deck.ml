type st =
  | BOMBED
  | SAFE
  | ATTACKED
  | DEAD
  | SKIPPED
  | ATTACKER

type card_name = string

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
  directory : card_id list;
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

let make_directory2 (lst : card_rem list) : card_id list =
  let m c = { name = c.name; genre = c.genre } in
  List.map m lst

(* from_json *)
let from_json json =
  let cards_info = json |> get_cards_info in
  let cards_left = cards_info |> get_rems_start [] in
  let directory = make_directory2 cards_info in
  let d_of_json j =
    {
      cards_used : card_id list = [];
      cards_left : card_id list;
      cards_info : card_rem list;
      directory : card_id list;
    }
  in
  d_of_json json

let decrease_rem_info lst card_name =
  let f ele =
    if ele.name = card_name then { ele with copies = ele.copies - 1 }
    else ele
  in
  List.map f lst

let rec get_defuse acc d player =
  match d.cards_left with
  | [] -> failwith "not possible"
  | h :: rest ->
      if h.genre <> "defuse" then
        let d = { d with cards_left = rest } in
        let acc = h :: acc in
        get_defuse acc d player
      else
        let cards_rem = decrease_rem_info d.cards_info h.name in
        let d = { d with cards_left = acc @ rest } in
        let d = { d with cards_info = cards_rem } in
        let player = { player with hand = [ h ] } in
        (d, player)

let rec draw_card_safe d player n =
  let hand = player.hand in
  match n with
  | 0 -> (d, player)
  | n_l -> (
      match d.cards_left with
      | h :: t ->
          if h.genre <> "bomb" then
            let cards_rem = decrease_rem_info d.cards_info h.name in
            let d = { d with cards_left = t } in
            let d = { d with cards_info = cards_rem } in
            let p = { player with hand = h :: hand } in
            draw_card_safe d p (n_l - 1)
          else
            let d = { d with cards_left = t @ [ h ] } in
            draw_card_safe d player n_l
      | _ -> failwith "Not possible")

(* shuffle
   (https://stackoverflow.com/questions/15095541/how-to-shuffle-list-in-on-in-ocaml) *)
let shuffle l =
  let nd = List.map (fun c -> (Random.bits (), c)) l in
  let sond = List.sort compare nd in
  List.map snd sond

let rec players_init acc num_player =
  match num_player with
  | 0 -> acc
  | n ->
      let acc = { hand = []; id = n - 1; state = SAFE } :: acc in
      players_init acc (n - 1)

let value (c : card_id) =
  if c.genre = "defuse" then 3
  else if c.genre = "functionals" then 2
  else 1

let card_compare c1 c2 =
  let c1_val = value c1 in
  let c2_val = value c2 in
  c1_val - c2_val

let sort_palyers_hands (t : t) =
  let d = fst t in
  let p = snd t in
  let f player =
    { player with hand = List.sort card_compare player.hand }
  in
  let p = { p with ai = List.map f p.ai } in
  let p = { p with user = f p.user } in
  (d, p)

let game_start_one_player d player_id =
  let d = { d with cards_left = shuffle d.cards_left } in
  let tup =
    get_defuse [] d { hand = []; id = player_id; state = SAFE }
  in
  let d = fst tup in
  let user = snd tup in
  draw_card_safe d user 7

let rec game_start_helper acc d num_player =
  match num_player with
  | 0 -> (d, acc)
  | n ->
      let res = game_start_one_player d (n - 1) in
      let d = fst res in
      let player = snd res in
      let acc = player :: acc in
      let num_player = num_player - 1 in
      game_start_helper acc d num_player

let game_start d num_player =
  let res = game_start_helper [] d num_player in
  let players = snd res in
  let d = fst res in
  match players with
  | [] -> failwith "violate precondition"
  | h :: t ->
      let user = h in
      let ai = t in
      let p = { ai; user } in
      sort_palyers_hands (d, p)

let get_hand t =
  let player = (snd t).user in
  player.hand

let cards_left t =
  let d = fst t in
  d.cards_left

let cards_used t =
  let d = fst t in
  d.cards_used

let cards_info t =
  let d = fst t in
  d.cards_info

let num_cards d =
  let f acc ele = acc + ele.copies in
  List.fold_left f 0 d.cards_info

let num_alive p =
  let f ele = ele.state != DEAD in
  List.filter f p.ai |> List.length

let change_state (t : t) player_id state : t =
  let d = fst t in
  let p = snd t in
  let ai = p.ai in
  let user = p.user in
  if player_id = 0 then
    let user = { user with state } in
    (d, { p with user })
  else
    let ai =
      let f ele =
        if ele.id = player_id then { ele with state } else ele
      in
      List.map f ai
    in
    (d, { p with ai })

let find_player p player_id =
  if player_id = 0 then p.user
  else
    let f ele = ele.id = player_id in
    List.filter f p.ai |> List.hd

(*updates the given player_id in p with argued [player] info *)
let mutate_p p player player_id =
  if player_id = 0 then { p with user = player }
  else
    let f ele = if ele.id = player_id then player else ele in
    { p with ai = List.map f p.ai }

let draw_card (t : t) player_id : t * string =
  let d = fst t in
  let p = snd t in
  let player = find_player p player_id in
  let hand = player.hand in
  match d.cards_left with
  | h :: rest ->
      if h.genre <> "bomb" then
        (* update cards_rem, cards_left, and the player's hand *)
        let cards_rem = decrease_rem_info d.cards_info h.name in
        let d = { d with cards_left = rest } in
        let d = { d with cards_info = cards_rem } in
        let player = { player with hand = h :: hand } in
        let p = mutate_p p player player_id in
        ((d, p), h.name) (* else change_state t player_id BOMBED *)
      else
        (* don't update anything -- just return the exploding kitten *)
        ((d, p), h.name)
  | _ -> failwith "Not possible"

let have_card hand name =
  List.mem name
    (let f (ele : card_id) = ele.name in
     List.map f hand)

let get_card (hand : card_id list) name : card_id =
  let f (ele : card_id) = ele.name = name in
  List.filter f hand |> List.hd

let rec remove acc (hand : card_id list) name =
  match hand with
  | [] -> acc
  | h :: rest ->
      if h.name = name then acc @ rest
      else
        let acc = h :: acc in
        remove acc rest name

let rec use_card t player_id name (num : int) =
  let d = fst t in
  let p = snd t in
  let player = find_player p player_id in
  let hand = player.hand in
  (* let count = ref num in *)
  (* let use_card_helper = *)
  if have_card hand name then
    let card = get_card hand name in
    let player = { player with hand = remove [] player.hand name } in
    let d = { d with cards_used = card :: d.cards_used } in
    let p = mutate_p p player player_id in
    if num > 0 then use_card t player_id name (num - 1) else (d, p)
  else t

(* in *)
(* while !count > 0 do use_card_helper done *)

let transfer_card t player_id1 player_id2 name =
  let d = fst t in
  let p = snd t in
  let player1 = find_player p player_id1 in
  let player2 = find_player p player_id2 in
  let hand1 = player1.hand in
  if have_card hand1 name then
    let card = get_card hand1 name in
    let player1 = { player1 with hand = remove [] player1.hand name } in
    let player2 = { player1 with hand = card :: player2.hand } in
    let p = mutate_p p player1 player1.id in
    let p = mutate_p p player2 player2.id in
    ((d, p), true)
  else (t, false)

(* returns a random card name from the gievn player's hand *)
let random_card (t : t) (player_id : int) : string =
  let player = find_player (snd t) player_id in
  let hand = player.hand in
  let max = (hand |> List.length) - 1 in
  let card = List.nth hand (Random.int max) in
  card.name

let transfer_card_rand t player_id1 player_id2 =
  (* get a random card name from p1 *)
  let name = random_card t player_id1 in
  let t, passed = transfer_card t player_id1 player_id2 name in
  (t, name)

let take_card t player_id name =
  let d = fst t in
  let p = snd t in
  let player = find_player p player_id in
  if have_card d.cards_used name then
    let card = get_card d.cards_used name in
    let d = { d with cards_used = remove [] d.cards_used name } in
    let player = { player with hand = card :: player.hand } in
    let p = mutate_p p player player.id in
    (d, p)
  else t

let player_have_card t player_id card_name =
  let p = snd t in
  let player = find_player p player_id in
  have_card player.hand card_name

let used_have_card t card_name =
  let d = fst t in
  let used = d.cards_used in
  have_card used card_name

let check_state t player_id : st =
  let p = snd t in
  let player = find_player p player_id in
  player.state

(* returns the number of copies of the named card in the given hand*)
let num_copies (t : t) player_id name : int =
  (* let d = fst t in *)
  let p = snd t in
  let player = find_player p player_id in
  let hand = player.hand in
  (* let count = ref 0 in *)
  (* let count = ref num in *)
  (* let use_card_helper = *)
  let f (ele : card_id) = ele.name = name in
  List.filter f hand |> List.length

let get_genre (t : t) (name : string) : string =
  (* let f (ele : card_id) = ele.name = name in let filted_list = json
     |> member "cards" |> to_list |> List.map in *)
  let d = fst t in
  let directory = d.directory in
  let f (ele : card_id) = ele.name = name in
  let spec_card = List.filter f directory |> List.hd in
  spec_card.genre

let get_one_card_rem c =
  {
    name = c |> member "name" |> to_string;
    genre = c |> member "genre" |> to_string;
  }

let make_directory json =
  json |> member "cards" |> to_list |> List.map get_one_card_rem

let is_id t player_id : bool =
  let p = snd t in
  let f (ele : player) = ele.id = player_id in
  List.filter f p.ai |> List.length > 0

let is_ai t player_id : bool = player_id != 0 && is_id t player_id

let is_card (t : t) (name : string) : bool =
  let directory = (fst t).directory in
  let f (ele : card_id) = ele.name = name in
  List.exists f directory

let peek (t : t) (num : int) : card_id list =
  let d = fst t in
  let pile = d.cards_left in
  let temp = ref [] in
  let count = ref num in
  while !count > 0 do
    temp := !temp @ [ List.nth pile (num - !count) ];
    count := !count - 1
  done;
  !temp

let peek_print t num : unit =
  let d = fst t in
  let pile = d.cards_left in
  (* let temp = ref [] in *)
  let count = ref num in
  while !count > 0 do
    (* temp := !temp @ [ List.nth pile (num - !count) ]; *)
    let card = List.nth pile (num - !count) in
    print_endline card.name;
    count := !count - 1
  done

(* [insert_at x n lst] inserts [x] at index [n] of [lst]. If n > length
   of list, x is placed at the list's tail *)
let rec insert_at x n = function
  | [] -> [ x ]
  | h :: t as l -> if n = 0 then x :: l else h :: insert_at x (n - 1) t

let place_bomb (t : t) index : t =
  let d = fst t in
  let bomb = List.hd d.cards_left in
  let tail = List.tl d.cards_left in
  let cards_left = insert_at bomb index tail in
  let d = { d with cards_left } in
  (d, snd t)
