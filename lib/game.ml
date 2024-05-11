open Foundation
open Stockwaste
open Tableau

(* open Random *)
open Card

(* AF: The game (t) is a record of the form: { f : Foundation.t; s :
   Stockwaste.t; b : Tableau.t } where the first entry is a foundation, the
   second entry is a stockwaste, and the third entry is a tableau. The
   respective abstraction functions are located in stockwaste.ml, tableau.ml and
   foundation.ml*)
(* RI: None.*)

type t = {
  f : Foundation.t;
  s : Stockwaste.t;
  b : Tableau.t;
  counter : int;
  undos : int;
  previous : t list;
  start_time : float;
  draw_style : string option;
}

let game_from_parts fn sn bn =
  {
    f = fn;
    s = sn;
    b = bn;
    counter = 0;
    undos = 0;
    previous = [];
    start_time = Unix.gettimeofday ();
    draw_style = None;
  }

let update_three_opt o g =
  match o with
  | None | Some "3" -> { g with draw_style = o }
  | _ -> raise (Failure "three-opt must be None or Some 3")

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
    let selected_index = Random.int (List.length lst) in
    let selected_element = List.nth lst selected_index in
    let remaining_elements = List.filter (fun x -> x <> selected_element) lst in
    select_random_elements (k - 1) remaining_elements (selected_element :: acc)


let new_game () =
  let card_data = shuffle_list generate_deck in


  let foundation = Foundation.initialize in


  let selected_cards, remaining_cards =
    select_random_elements 28 card_data []
  in
  let tableau = Tableau.init_tab (shuffle_list selected_cards) in

  let a = add_sw (shuffle_list remaining_cards) empty_sw in

  game_from_parts foundation a tableau

(* Call this with the previous game state and desired component modifications at
   the end of a successful move to modify it accordingly, increment the counter,
   and add the previous game state to [previous]*)
let routine game f s b =
  {
    game with
    f;
    s;
    b;
    counter = game.counter + 1;
    previous = game :: game.previous;
  }

(* Gets the previous game with the modification that the number of undos is now
   the number of undos of the current game + 1*)
let undo game =
  match game.previous with
  | [] ->
      (game, Some "This is the original game, there is nothing left to undo.")
  | h :: _ -> ({ h with undos = game.undos + 1 }, None)


let draw_card fsb =
  match draw (fun () -> fsb.draw_style) fsb.s with
  | None ->
      ( fsb,
        Some
          "There are no cards in the waste or stock, \n\
          \    so this move is not valid." )
  | Some h -> (routine fsb fsb.f h fsb.b, None)

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
      if valid_move foundation card then
        ( routine game (put foundation card)
            (remove_opt (remove_top game.s))
            game.b,
          None )
      else (game, Some "This card cannot go in the foundation.")

let valid_stock_to_tableau_movement game tab_index card =
  let tableau = game.b in
  try
    ( routine game game.f
        (remove_opt (remove_top game.s))
        (card_to_col tableau tab_index card),
      None )
  with IllegalMove ->
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
  let updated_game = routine game updated_foundation game.s updated_tableau in
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

          let final_game = routine game updated_foundation game.s t in
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
      if num_of top_c = 13 then update_game_with_move game tab_index top_c
      else (game, Some "You can not make this move.")
  | top_card, Some c ->
      if num_of top_card = 0 then
        (game, Some "There is no card in this foundation column.")
      else if num_of top_card - num_of c = -1 && color_of c <> color_of top_card
      then update_game_with_move game tab_index top_card
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
    | newb -> (routine g g.f g.s newb, None)

let check_win g = is_complete g.f
let won_game = game_from_parts won_foundation empty_sw empty_tab

let autowin g =
  if check_stock_empty g.s && top_sw g.s = None && winnable g.b then
    ( {
        won_game with
        start_time = g.start_time;
        undos = g.undos;
        counter =
          g.counter
          + List.fold_left
              (fun acc lst -> acc + List.length lst)
              0 (to_cd_lst g.b);
      },
      None )
  else
    ( g,
      Some
        "This game is not autowinnable. The stockwaste must be empty and all \
         tableau cards must be visible." )

let rec autowin_gamelist_helper g lst =
  match lowest_col_index g.b with
  | None -> if List.length lst > 0 then List.rev (List.tl lst) else lst
  | Some i ->
      let newg = fst (move_tableau_card_to_foundation g i) in
      autowin_gamelist_helper newg (newg :: lst)

let autowin_gamelist g = autowin_gamelist_helper g []

let cheat g coli cardi =
  let coli = int_of_string_opt coli in
  let cardi = int_of_string_opt cardi in
  if coli = None || cardi = None then (g, Some "Unrecognizable Command.")
  else
    match cheat_col_card g.b (Option.get coli - 1) (Option.get cardi) with
    | exception InvalidColID -> (g, Some "Invalid Column ID.")
    | exception InvalidCardID -> (g, Some "Invalid Card ID.")
    | card -> (g, Some ("The specified card is " ^ to_string card ^ "."))

let get_count g = g.counter
let get_undos g = g.undos
let start_time g = g.start_time
