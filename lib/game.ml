open Foundation
open Stockwaste
open Tableau
open Random

type t = {
  f : Foundation.t;
  s : Stockwaste.t;
  b : Tableau.t;
}

let card_data = BatList.of_enum (BatFile.lines_of "data/card.txt")

(* let shuffle_list lst = let compare_random _ _ = Random.int 2 in let shuffled
   = List.sort compare_random lst in shuffled *)

let rec select_random_elements k lst acc =
  if k = 0 then (acc, lst)
  else
    match lst with
    | [] -> (acc, [])
    | _ ->
        let selected_index = Random.int (List.length lst) in
        let selected_element = List.nth lst selected_index in
        let remaining_elements =
          List.filter (fun x -> x <> selected_element) lst
        in
        select_random_elements (k - 1) remaining_elements
          (selected_element :: acc)

let selected_elements, remaining_elements =
  select_random_elements 28 card_data []

(** read csv file containing card names and initialize game. foundation must be
    empty, and tableau adds 28 cards and the rest goes to Stockwaste *)
(*let newgame game = game.f = f.initialize ; game.s = empty_sw ; game.b =b *)

(**[draw_card fsb] draws a card and moves it from the stock to the waste in
   stockwaste (s)*)
let draw_card fsb = { f = fsb.f; s = draw fsb.s; b = fsb.b }

let formatted fsb =
  let s_empty = check_stock_empty fsb.s in
  let s_cds = get_waste fsb.s in
  let f_cds = top_cards fsb.f in
  let b_cds = to_cd_lst fsb.b in
  ((s_empty, s_cds), f_cds, b_cds)
