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
let three_opt = ref None
let counter = ref 0
let previous = ref []
let undos = ref 0

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
  counter := 0;
  previous := [];
  undos := 0;
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

let game_from_parts fn sn bn = { f = fn; s = sn; b = bn }

(* Call this with the previous game state at the end of a successful move to
   increment the counter and add the previous game state to [previous]*)
let routine game =
  incr counter;
  previous := game :: !previous

(* Can be used to reverse the effects of routine.*)
let partial_undo () =
  match !previous with
  | [] -> failwith "No previous game state to return to."
  | h :: t ->
      previous := t;
      decr counter;
      h

let undo game =
  try
    let g2 = partial_undo () in
    incr undos;
    (g2, None)
  with Failure _ ->
    (game, Some "This is the original game, there is nothing left to undo.")

(**[draw_card fsb] draws a card and moves it from the stock to the waste in
   stockwaste (s)*)
let draw_card fsb =
  match draw (fun () -> !three_opt) fsb.s with
  | None ->
      ( fsb,
        Some
          "There are no cards in the waste or stock, \n\
          \    so this move is not valid." )
  | Some h ->
      routine fsb;
      ({ f = fsb.f; s = h; b = fsb.b }, None)

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
          "The stock is empty, so it is not possible to move a card to the \
           foundation from it." )
  | Some card ->
      let foundation = game.f in
      if valid_move foundation card then (
        routine game;
        ( {
            f = put foundation card;
            s = remove_opt (remove_top game.s);
            b = game.b;
          },
          None ))
      else (game, Some "This card cannot go in the foundation.")

let valid_stock_to_tableau_movement game tab_index card =
  let tableau = game.b in
  try
    routine game;
    ( {
        f = game.f;
        s = remove_opt (remove_top game.s);
        b = card_to_col tableau tab_index card;
      },
      None )
  with IllegalMove ->
    ignore (partial_undo ());
    ( game,
      Some
        ("This card cannot go in column "
        ^ string_of_int (tab_index + 1)
        ^ ". (Illegal Move)") )

let s_to_t (game : t) tab_index =
  if tab_index < 0 || tab_index > 6 then
    ( game,
      Some
        (string_of_int (tab_index + 1)
        ^ " is not a valid index in the tableau. Must be from 1 to 7.") )
  else
    match top_sw game.s with
    | None ->
        ( game,
          Some
            "The stock is empty, so it is not possible to move a card to the \
             foundation from it." )
    | Some card -> valid_stock_to_tableau_movement game tab_index card

let update_game_with_move game tab_index foundation_card =
  let updated_tableau = card_to_col game.b tab_index foundation_card in
  let updated_foundation = remove game.f foundation_card in
  let updated_game =
    { f = updated_foundation; s = game.s; b = updated_tableau }
  in
  (updated_game, None)

let move_tableau_card_to_foundation game col_index =
  if col_index >= 0 && col_index <= 6 then
    match peek_col_card game.b col_index with
    | None -> (game, Some "No card is present here.")
    | Some card ->
        let foundation = game.f in
        let _ = suit_of card in
        let valid_move_to_foundation = valid_move foundation card in
        if valid_move_to_foundation then
          let updated_foundation = put foundation card in

          let t, _ = pop_col_card game.b col_index in

          let final_game =
            routine game;
            { f = updated_foundation; s = game.s; b = t }
          in
          (final_game, None)
        else (game, Some "You can not make this move.")
  else (game, Some (string_of_int col_index ^ " is not a valid index."))

let validate_foundation_index game found_index =
  ( game,
    Some (string_of_int found_index ^ " is not a valid index in the foundation.")
  )

let validate_tab_index game tab_index =
  ( game,
    Some (string_of_int tab_index ^ " is not a valid index in the tableau.") )

let valid_foundation_to_tableau_movement game tab_index foundation_columns
    found_index =
  let card = peek_col_card game.b tab_index in
  let foundation_card = List.nth foundation_columns found_index in
  match (foundation_card, card) with
  | top_c, None ->
      if num_of top_c = 13 then (
        routine game;
        update_game_with_move game tab_index top_c)
      else (game, Some "You can not make this move.")
  | top_card, Some c ->
      if num_of top_card = 0 then
        (game, Some "There is no card in this foundation column.")
      else if num_of top_card - num_of c = -1 && color_of c <> color_of top_card
      then (
        routine game;
        update_game_with_move game tab_index top_card)
      else (game, Some "You can not make this move.")

let move_card_from_foundation_to_tableau game found_index tab_index =
  let find_and_move foundation_columns found_index tab_index =
    if found_index = -1 then (game, Some "Foundation index is not valid.")
    else if found_index < 0 || found_index > 3 then
      validate_foundation_index game found_index
    else if tab_index < 0 || tab_index > 6 then
      validate_tab_index game tab_index
    else
      valid_foundation_to_tableau_movement game tab_index foundation_columns
        found_index
  in
  find_and_move (top_cards game.f) found_index tab_index

let t_to_t g c1 c2 =
  let nc1 = int_of_string_opt c1 in
  let nc2 = int_of_string_opt c2 in
  if nc1 = None || nc2 = None then (g, Some "Unrecognizable Command.")
  else
    match move_col_to_col g.b (Option.get nc1 - 1) (Option.get nc2 - 1) with
    | exception InvalidColID -> (g, Some "Invalid Column ID.")
    | exception IllegalMove -> (g, Some "Illegal Move.")
    | exception _ -> (g, Some "Unknown error from tableau.ml.")
    | newb ->
        routine g;
        ({ f = g.f; s = g.s; b = newb }, None)

let check_win g = is_complete g.f
let won_game = { f = won_foundation; s = empty_sw; b = empty_tab }

let autowin g =
  if check_stock_empty g.s && top_sw g.s = None && winnable g.b then
    (won_game, None)
  else
    ( g,
      Some
        "This game is not autowinnable. The stockwaste must be empty and all \
         tableau cards must be visible." )

let cheat g coli cardi =
  let coli = int_of_string_opt coli in
  let cardi = int_of_string_opt cardi in
  if coli = None || cardi = None then (g, Some "Unrecognizable Command.")
  else
    match cheat_col_card g.b (Option.get coli - 1) (Option.get cardi) with
    | exception InvalidColID -> (g, Some "Invalid Column ID.")
    | exception InvalidCardID -> (g, Some "Invalid Card ID.")
    | exception _ -> (g, Some "Unknown error from tableau.ml.")
    | card -> (g, Some ("The specified card is " ^ to_string card ^ "."))

let update_three_opt o =
  match o with
  | None | Some "3" -> three_opt := o
  | _ -> raise (Failure "three-opt must be None or Some 3")

let get_count () = !counter
let get_undos () = !undos
