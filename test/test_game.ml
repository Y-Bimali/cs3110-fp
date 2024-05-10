open OUnit2
open Fp.Card
open Fp.Tableau
open Fp.Stockwaste
open Fp.Foundation
open Fp.Game

let h1 = new_card Hearts 1
let h2 = new_card Hearts 2
let d1 = new_card Diamonds 1
let d2 = new_card Diamonds 2
let d3 = new_card Diamonds 3
let d4 = new_card Diamonds 4
let d5 = new_card Diamonds 5
let d6 = new_card Diamonds 6
let d7 = new_card Diamonds 7
let d8 = new_card Diamonds 8
let d9 = new_card Diamonds 9
let d10 = new_card Diamonds 10
let d11 = new_card Diamonds 11
let d12 = new_card Diamonds 12
let d13 = new_card Diamonds 13
let c1 = new_card Clubs 1
let c3 = new_card Clubs 3
let c7 = new_card Clubs 7
let c9 = new_card Clubs 9
let c10 = new_card Clubs 10
let c11 = new_card Clubs 11
let s1 = new_card Spades 1
let s2 = new_card Spades 2
let s10 = new_card Spades 10

(*Test case 1 with these properties: S to F: Can move Ace from Stock
  (stockwaste1) into Foundation (foundation1 or foundation2) ,S to T: Can move
  Ace from Stock (stockwaste2) into Tableau (tableau1), T to F: Can move Ace
  from Tableau (tableau1 - col 2) into Foundation (foundation1 or foundation2) ,
  T to T: Can move Ace from Tableau(tableau1 - col 2) to Tableau(tableau1 - col
  3), T to T: Can move 8,7 from Tableau (tableau 1- col 1) to Tableau(tableau1 -
  col 0), F to T: Can move 2 from Foundation (foundation2) to Tableau (tableau1
  - col 4) *)

let foundation1 = put initialize h1
let foundation2 = put foundation1 h2

let stockwaste1 =
  Option.get (draw (fun () -> None) (add_sw [ s1; s2 ] empty_sw))

let stockwaste2 = Option.get (draw (fun () -> None) (add_sw [ d8 ] empty_sw))

let tableau1 =
  init_tab
    (List.flatten
       [
         [ d1; d1; d1; d1; d1; d1; d1 ];
         [ d1; d1; d1; d1; d1; d1 ];
         [ c3; c3; c3; c3; c3 ];
         [ h2; h2; h2; h2 ];
         [ c1; c1; c1 ];
         [ c7; d8 ];
         [ c9 ];
       ])

(*Test case 2 with these properties: S to F: Can not move King from Stock
  (stockwaste4) into Foundation(foundation1 or foundation2), S to T: Can not
  move Queen from Stock (stockwaste3) into Tableau (tableau2 - col 0), T to F:
  Can not move Jack from Tableau (tableau11 - col 2) into Foundation
  (foundation1 or foundation2),T to T: Can not move 10 from Tableau (tableau1-
  col 0) ) to Tableau(tableau1 - col 3) T to T: Can not move Jack,10 from
  Tableau (tableau1 - col 1) to Tableau (tableau1 - col 0) F to T: Can not move
  Ace from Foundation(foundation1) to Tableau(tableau1 - col 1) *)

let stockwaste3 =
  Option.get (draw (fun () -> None) (add_sw [ d12; d1 ] empty_sw))

let stockwaste0 = empty_sw
let stockwaste4 = Option.get (draw (fun () -> None) (add_sw [ d13 ] empty_sw))

let tableau2 =
  init_tab
    (List.flatten
       [
         [ d1; d1; d1; d1; d1; d1; d1 ];
         [ d1; d1; d1; d1; d1; d1 ];
         [ c3; c3; c3; c3; c3 ];
         [ h2; h2; h2; h2 ];
         [ c11; c11; c11 ];
         [ s10; d11 ];
         [ c10 ];
       ])

let test_s_to_ft_works _ =
  let g1 = game_from_parts foundation1 stockwaste1 tableau1 in
  let g2 = game_from_parts foundation2 stockwaste1 tableau1 in
  let g3 = game_from_parts foundation1 stockwaste2 tableau1 in
  assert_equal (snd (s_to_f g1)) None;
  assert_equal (snd (s_to_f g2)) None;
  assert_equal (snd (s_to_t g3 0)) None

let test_s_to_ft_err _ =
  let g0 = game_from_parts foundation1 stockwaste0 tableau2 in
  let g1 = game_from_parts foundation1 stockwaste4 tableau2 in
  let g2 = game_from_parts foundation2 stockwaste4 tableau2 in
  let g3 = game_from_parts foundation1 stockwaste3 tableau2 in
  let g4 = game_from_parts foundation1 stockwaste3 tableau2 in
  assert_equal (snd (s_to_f g1)) (Some "This card cannot go in the foundation.");
  assert_equal
    (snd (s_to_f g0))
    (Some
       "The stock is empty, so it is not possible to move a card to the \
        foundation from it.");
  assert_equal
    (snd (s_to_t g0 2))
    (Some
       "The stock is empty, so it is not possible to move a card to the \
        foundation from it.");
  assert_equal (snd (s_to_f g2)) (Some "This card cannot go in the foundation.");
  assert_equal
    (snd (s_to_t g3 2))
    (Some
       ("This card cannot go in column " ^ string_of_int 3 ^ ". (Illegal Move)"));
  assert_equal
    (snd (s_to_t g4 20))
    (Some
       (string_of_int 21
      ^ " is not a valid index in the tableau. Must be from 1 to 7."));
  assert_equal
    (snd (s_to_t g4 (-2)))
    (Some "-1 is not a valid index in the tableau. Must be from 1 to 7.")

let tableau3 =
  init_tab
    (List.flatten
       [
         [ d1; d1; d1; d1; d1; d1; d1 ];
         [ d8; d1; d1; d1; d1; d12 ];
         [ c3; c3; c3; c3; c3 ];
         [ h2; h2; h2; h2 ];
         [ c1; d1; s1 ];
         [ s2; s2 ];
         [ h1 ];
       ])

let foundation3 = initialize

let test_tableau_to_foundation_works _ =
  let g1 = game_from_parts foundation3 stockwaste0 tableau3 in
  let updated_g1 = move_tableau_card_to_foundation g1 0 in
  let updated_g2 = move_tableau_card_to_foundation (fst updated_g1) 0 in

  assert_equal (snd updated_g1) None;
  assert_equal (snd updated_g2) (Some "No card is present here.");
  assert_equal
    (snd (move_tableau_card_to_foundation g1 (-2)))
    (Some "-2 is not a valid index.");
  assert_equal
    (snd (move_tableau_card_to_foundation g1 5))
    (Some "You can not make this move.");
  assert_equal (snd (move_tableau_card_to_foundation g1 6)) None;
  assert_equal
    (snd (move_card_from_foundation_to_tableau (fst updated_g2) 1 1))
    None;
  assert_equal
    (snd (move_card_from_foundation_to_tableau (fst updated_g2) 1 0))
    (Some "You can not make this move.");
  assert_equal
    (snd (move_card_from_foundation_to_tableau (fst updated_g2) 0 6))
    (Some "There is no card in this foundation column.");
  assert_equal
    (snd (move_card_from_foundation_to_tableau (fst updated_g2) 1 6))
    (Some "You can not make this move.");
  assert_equal
    (snd (move_card_from_foundation_to_tableau (fst updated_g2) 4 6))
    (Some "4 is not a valid index in the foundation.");
  assert_equal
    (snd (move_card_from_foundation_to_tableau (fst updated_g2) (-2) 6))
    (Some "-2 is not a valid index in the foundation.");
  assert_equal
    (snd (move_card_from_foundation_to_tableau (fst updated_g2) (-1) 6))
    (Some "Foundation index is not valid.");
  assert_equal
    (snd (move_card_from_foundation_to_tableau (fst updated_g2) 1 (-2)))
    (Some "-2 is not a valid index in the tableau.");
  assert_equal
    (snd (move_card_from_foundation_to_tableau (fst updated_g2) 1 8))
    (Some "8 is not a valid index in the tableau.")

let tableau4 =
  init_tab
    (List.flatten
       [
         [ d4; d5; d6; d7; d8; d9; d10 ];
         [ s10; d1; d1; d1; d1; s10 ];
         [ c3; c3; c3; c3; c3 ];
         [ h2; h2; h2; h2 ];
         [ d11; d12; d13 ];
         [ d2; d3 ];
         [ d1 ];
       ])

let test_king_movement _ =
  let g1 = game_from_parts foundation3 stockwaste0 tableau4 in
  let updated_g1 = move_tableau_card_to_foundation g1 0 in

  let updated_g2 = move_tableau_card_to_foundation (fst updated_g1) 1 in
  let updated_g3 = move_tableau_card_to_foundation (fst updated_g2) 1 in
  let updated_g4 = move_tableau_card_to_foundation (fst updated_g3) 6 in
  let updated_g5 = move_tableau_card_to_foundation (fst updated_g4) 6 in
  let updated_g6 = move_tableau_card_to_foundation (fst updated_g5) 6 in
  let updated_g7 = move_tableau_card_to_foundation (fst updated_g6) 6 in
  let updated_g8 = move_tableau_card_to_foundation (fst updated_g7) 6 in
  let updated_g9 = move_tableau_card_to_foundation (fst updated_g8) 6 in
  let updated_g10 = move_tableau_card_to_foundation (fst updated_g9) 6 in
  let updated_g11 = move_tableau_card_to_foundation (fst updated_g10) 2 in
  let updated_g12 = move_tableau_card_to_foundation (fst updated_g11) 2 in
  let updated_g13 = move_tableau_card_to_foundation (fst updated_g12) 2 in
  let updated_g14 =
    move_card_from_foundation_to_tableau (fst updated_g13) 3 2
  in
  assert_equal (snd updated_g14) None

let test_counter _ =
  let g = new_game () in
  let g1 = draw_card g |> fst in
  let g2 = draw_card g1 |> fst in
  assert_equal (get_count g) 0;
  assert_equal (get_count g1) 1;
  assert_equal (get_count g2) 2

let test_undo _ =
  let g1 = game_from_parts foundation1 stockwaste1 tableau1 in
  let g2 = fst (s_to_f g1) in
  assert_equal (undo g2 |> snd) None;
  let g3 = fst (undo g2) in
  assert_equal (formatted g3) (formatted g1);
  (* They are equal except for the undo value *)
  assert_equal (get_undos g3) 1;
  assert_equal (get_count g3) 0;
  let g4 = new_game () in
  assert_equal (undo g4)
    (g4, Some "This is the original game, there is nothing left to undo.");
  assert_equal (get_undos g4) 0

(*let tab_tests = "Tableau only tests" >::: [ "test_t_to_t" >:: test_t_to_t ]*)
let test_t_to_t _ =
  let g = game_from_parts foundation1 stockwaste0 tableau1 in
  assert_equal (snd (t_to_t g "1" "1")) None;
  let g2 = game_from_parts foundation1 stockwaste0 tableau3 in
  assert_equal (snd (t_to_t g2 "3" "4")) None;
  assert_equal (snd (t_to_t (fst (t_to_t g2 "3" "4")) "4" "5")) None;
  assert_equal
    (snd (t_to_t (fst (t_to_t (fst (t_to_t g2 "3" "4")) "4" "5")) "7" "2"))
    None;
  assert_equal (snd (t_to_t g "2" "5")) (Some "Illegal Move.");
  assert_equal (snd (t_to_t g "9" "8")) (Some "Invalid Column ID.");
  assert_equal (snd (t_to_t g "ai49" "3")) (Some "Unrecognizable Command.");
  assert_equal (snd (t_to_t g "3" "nwo")) (Some "Unrecognizable Command.")

let test_check_win _ =
  assert_equal (check_win won_game) true;
  assert_equal
    (check_win (game_from_parts foundation1 stockwaste0 tableau1))
    false

let foundation_queens =
  set (new_card Spades 12) (new_card Hearts 12) (new_card Clubs 12)
    (new_card Diamonds 12)

let tableau_kings =
  List.fold_left2
    (fun acc a b -> card_to_col acc a b)
    empty_tab [ 0; 1; 2; 3 ]
    [
      new_card Spades 13;
      new_card Hearts 13;
      new_card Diamonds 13;
      new_card Clubs 13;
    ]

let test_autowin _ =
  assert_equal
    (autowin (game_from_parts foundation_queens stockwaste0 tableau_kings))
    (won_game, None);
  let g1 = game_from_parts foundation1 stockwaste0 tableau1 in
  assert_equal (autowin g1)
    ( g1,
      Some
        "This game is not autowinnable. The stockwaste must be empty and all \
         tableau cards must be visible." )

let test_autowin_gamelist _ =
  assert_equal (autowin_gamelist won_game) [];
  assert_equal
    (List.length
       (autowin_gamelist
          (game_from_parts foundation_queens stockwaste0 tableau_kings)))
    3

let test_cheat _ =
  let g2 = game_from_parts foundation1 stockwaste0 tableau2 in
  let g4 = game_from_parts foundation1 stockwaste0 tableau4 in
  assert_equal (cheat g2 "1" "1") (g2, Some "The specified card is 10♣.");
  assert_equal (cheat g4 "7" "7") (g4, Some "The specified card is 4♦.");
  assert_equal (cheat g4 "0" "1") (g4, Some "Invalid Column ID.");
  assert_equal (cheat g4 "1" "0") (g4, Some "Invalid Card ID.");
  assert_equal (cheat g4 "po" "0") (g4, Some "Unrecognizable Command.");
  assert_equal (cheat g4 "3" "weee") (g4, Some "Unrecognizable Command.")

let game_tests =
  "game_tests"
  >::: [
         "valid moves test_s_to_ft" >:: test_s_to_ft_works;
         "invalid moves test_s_to_ft" >:: test_s_to_ft_err;
         "test_tab_to_foundation" >:: test_tableau_to_foundation_works;
         "test_king_to_foundation" >:: test_king_movement;
         "test_counter" >:: test_counter;
         "test_undo" >:: test_undo;
         "test_t_to_t" >:: test_t_to_t;
         "test_check_win" >:: test_check_win;
         "test_autowin" >:: test_autowin;
         "test_autowin_gamelist" >:: test_autowin_gamelist;
         "test_cheat" >:: test_cheat;
       ]

let () = run_test_tt_main game_tests
