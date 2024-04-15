open Foundation
open Stockwaste
open Tableau

(* open Random *)
open Card

type t = {
  f : Foundation.t;
  s : Stockwaste.t;
  b : Tableau.t;
}

(* let card_data = BatList.of_enum (BatFile.lines_of "data/card.txt") *)

let generate_deck =
  let suits = [ Spades; Hearts; Clubs; Diamonds ] in
  let ranks = [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13 ] in
  let rec generate_cards suits ranks acc =
    match suits with
    | [] -> acc
    | suit :: rest_suits ->
        let cards_of_suit = List.map (fun rank -> new_card suit rank) ranks in
        generate_cards rest_suits ranks (acc @ cards_of_suit)
  in
  generate_cards suits ranks []

let shuffle_list lst =
  let compare_random _ _ = Random.int 2 in
  let shuffled = List.sort compare_random lst in
  shuffled

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

(**[new_game ()] initializes the game_state. foundation must be empty, and
   tableau adds 28 cards and the rest goes to Stockwaste *)

let new_game () =
  let card_data = shuffle_list generate_deck in

  (* Initialize Foundation *)
  let foundation = Foundation.initialize in

  (* Initialize Tableau *)
  let selected_cards, remaining_cards =
    select_random_elements 28 card_data []
  in
  let tableau = Tableau.init_tab (shuffle_list selected_cards) in

  let a = add_sw (shuffle_list remaining_cards) empty_sw in

  { f = foundation; s = a; b = tableau }

(**[draw_card fsb] draws a card and moves it from the stock to the waste in
   stockwaste (s)*)
let draw_card fsb =
  try ({ f = fsb.f; s = draw fsb.s; b = fsb.b }, None)
  with NoCards ->
    ( fsb,
      Some
        "There are no cards in the waste or stock, \n\
        \    so this move is not valid." )

let formatted fsb =
  let s_empty = check_stock_empty fsb.s in
  let s_cds = get_waste fsb.s in
  let f_cds = top_cards fsb.f in
  let b_cds = to_cd_lst fsb.b in
  ((s_empty, s_cds), f_cds, b_cds)

let s_to_f g = (g, Some "Hello")

(* tentative functions for interacting with foundation and tableau *)
let move_card_to_foundation game col_index =
  match peek_col_card game.b col_index with
  | None -> (game, None)
  | Some card ->
      let foundation = game.f in
      let _ = suit_of card in
      let valid_move_to_foundation = valid_move foundation card in
      if valid_move_to_foundation then
        let updated_foundation = put foundation card in

        let t, _ = pop_col_card game.b col_index in

        let final_game = { f = updated_foundation; s = game.s; b = t } in
        (final_game, None)
      else (game, Some "Invalid move")

(* in the future add case for adding king to empty tableau column *)
let move_matching_card_to_tableau game found_index tab_index =
  let find_and_move foundation_columns found_index tab_index =
    let card = peek_col_card game.b tab_index in

    let foundation_card = List.nth foundation_columns found_index in

    match (foundation_card, card) with
    | top_c, None ->
        if num_of top_c = 13 then
          try
            let updated_tableau = card_to_col game.b tab_index top_c in
            let updated_foundation = remove game.f top_c in
            let updated_game =
              { f = updated_foundation; s = game.s; b = updated_tableau }
            in
            (updated_game, None)
          with IllegalMove -> (game, Some "This move is illegal")
        else (game, Some "Can not move King there")
    | top_card, Some c ->
        if num_of top_card = 0 then (game, Some "The index here is empty")
        else if
          num_of top_card - num_of c = -1 && color_of c <> color_of top_card
        then
          try
            let updated_tableau = card_to_col game.b tab_index top_card in
            let updated_foundation = remove game.f top_card in
            let updated_game =
              { f = updated_foundation; s = game.s; b = updated_tableau }
            in
            (updated_game, None)
          with IllegalMove -> (game, Some "Illegal move")
        else (game, Some "Error")
  in

  find_and_move (top_cards game.f) found_index tab_index
