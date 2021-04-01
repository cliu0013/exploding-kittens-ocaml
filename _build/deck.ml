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

let from_json json =
  let parse_genres j =
    let rec map f = function [] -> [] | h :: t -> f h :: map f t in
    let kittens =
      let rec get_kittens v = function
        | [] -> []
        | h :: t ->
            let reps =
              h |> member "number" |> to_string |> int_of_string
            in
            let i = ref 0 in
            let acc = ref [] in
            let template =
              {
                name = h |> member "name" |> to_string;
                genre = "kittens";
              }
            in
            (* while !i < reps do acc := !acc @ [template]; incr i done *)
            let acc = ref [] in
            !acc :: get_kittens t
      in
      j |> member "kittens" |> to_list |> get_kittens
    in
    let fxns =
      let get_fxns v =
        {
          name = v |> member "name" |> to_string;
          genre = "functionals";
        }
      in
      j |> member "functionals" |> to_list |> List.map get_fxns
    in
    let diffuses =
      let get_diffuses v =
        { name = v |> member "name" |> to_string; genre = "diffuses" }
      in
      j |> member "diffuses" |> to_list |> List.map get_diffuses
    in
    let bombs =
      let get_bombs v =
        { name = v |> member "name" |> to_string; genre = "bombs" }
      in
      j |> member "bombs" |> to_list |> List.map get_bombs
    in
    kittens @ fxns @ diffuses @ bombs
  in
  let get_rems j = [] in
  let d_of_json j =
    {
      cards_used : card_id list = [];
      cards_left : card_id list = j |> parse_genres;
      cards_info : card_rem list = j |> get_rems;
    }
  in
  let parse j : d =
    try d_of_json j
    with Type_error (s, _) -> failwith ("Parsing error: " ^ s)
  in
  parse json

(* failwith "unimplemented" *)

let cards_start d = failwith "unimplemented"

let cards_left d = failwith "unimplemented"

let cards_used d = failwith "unimplemented"

let cards_info d = failwith "unimplemented"

let draw_card d = failwith "unimplemented"

let peak d = failwith "unimplemented"

let pop d = failwith "unimplemented"

let append d = failwith "unimplemented"
