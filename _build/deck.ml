(* Note: You may introduce new code anywhere in this file. *)

type card_id = {
  name : string;
  genre : string;
}

type card_rem = {
  name : string;
  copies : int;
}

type d = {
  cards_used : card_id list;
  cards_left : card_id list;
  cards_info : card_rem list;
}

(* TODO: replace [unit] with a type of your own design. *)
open Yojson.Basic.Util

let from_json json = failwith "unimplemented"

let cards_start d = failwith "unimplemented"

let cards_left d = failwith "unimplemented"

let cards_used d = failwith "unimplemented"

let cards_info d = failwith "unimplemented"

let draw_card d = failwith "unimplemented"

let peak d = failwith "unimplemented"

let pop d = failwith "unimplemented"

let append d = failwith "unimplemented"
