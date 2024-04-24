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
  match draw fsb.s with
  | None ->
      ( fsb,
        Some
          "There are no cards in the waste or stock, \n\
          \    so this move is not valid." )
  | Some h -> ({ f = fsb.f; s = h; b = fsb.b }, None)

let formatted fsb =
  let s_empty = check_stock_empty fsb.s in
  let s_cds = get_waste fsb.s in
  let f_cds = top_cards fsb.f in
  let b_cds = to_cd_lst fsb.b in
  ((s_empty, s_cds), f_cds, b_cds)

let remove_opt opt =
  match opt with
  | Some h -> h
  | None -> failwith "None"

let s_to_f (game : t) =
  match top_sw game.s with
  | None ->
      ( game,
        Some
          "The stock is empty, so it is not possible to move a \n\
          \  card to the foundation from it." )
  | Some card ->
      let foundation = game.f in
      if valid_move foundation card then
        ( {
            f = put foundation card;
            s = remove_opt (remove_top game.s);
            b = game.b;
          },
          None )
      else (game, Some "This card cannot go in the foundation.")

let s_to_t (game : t) tab_index =
  if tab_index < 0 || tab_index > 6 then
    ( game,
      Some
        (string_of_int tab_index
       ^ " is not a valid index in the tableau. Must be from 0 to 6") )
  else
    match top_sw game.s with
    | None ->
        ( game,
          Some
            "The stock is empty, so it is not possible to move a \n\
            \  card to the foundation from it." )
    | Some card -> (
        let tableau = game.b in
        try
          ( {
              f = game.f;
              s = remove_opt (remove_top game.s);
              b = card_to_col tableau tab_index card;
            },
            None )
        with IllegalMove ->
          ( game,
            Some
              ("This card cannot go in column " ^ string_of_int tab_index
             ^ ". (Illegal Move)") ))

let update_game_with_move game tab_index foundation_card =
  try
    let updated_tableau = card_to_col game.b tab_index foundation_card in
    let updated_foundation = remove game.f foundation_card in
    let updated_game =
      { f = updated_foundation; s = game.s; b = updated_tableau }
    in
    (updated_game, None)
  with IllegalMove -> (game, Some "Illegal move")

let move_card_to_foundation game col_index =
  if col_index >= 0 && col_index <= 6 then
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
  else (game, Some (string_of_int col_index ^ " is not a valid index"))

let validate_foundation_index game found_index =
  ( game,
    Some
      (string_of_int found_index
     ^ " is not a valid index in the foundation. Must be from 0 to 3") )

let validate_tab_index game tab_index =
  ( game,
    Some
      (string_of_int tab_index
     ^ " is not a valid index in the tableau. Must be from 0 to 6") )

let move_matching_card_to_tableau game found_index tab_index =
  let find_and_move foundation_columns found_index tab_index =
    if found_index < 0 || found_index > 3 then
      validate_foundation_index game tab_index
    else if tab_index < 0 || tab_index > 6 then
      validate_tab_index game tab_index
    else
      let card = peek_col_card game.b tab_index in
      let foundation_card = List.nth foundation_columns found_index in
      match (foundation_card, card) with
      | top_c, None ->
          if num_of top_c = 13 then update_game_with_move game tab_index top_c
          else (game, Some "Can not move this card there")
      | top_card, Some c ->
          if num_of top_card = 0 then (game, Some "The index here is empty")
          else if
            num_of top_card - num_of c = -1 && color_of c <> color_of top_card
          then update_game_with_move game tab_index top_card
          else (game, Some "You can not make this move")
  in
  find_and_move (top_cards game.f) found_index tab_index

let char_to_int c = int_of_string_opt (Char.escaped c)

let t_to_t g c1 c2 i =
  let nc1 = char_to_int c1 in
  let nc2 = char_to_int c2 in
  let ni = char_to_int i in
  if nc1 = None || nc2 = None || ni = None then
    (g, Some "Unrecognizable Command.")
  else
    match
      move_col_to_col g.b (Option.get nc1) (Option.get nc2) (Option.get ni)
    with
    | exception InvalidColID -> (g, Some "Invalid Column ID.")
    | exception IllegalMove -> (g, Some "Illegal Move.")
    | newb -> ({ f = g.f; s = g.s; b = newb }, None)

let check_win g = is_complete g.f
