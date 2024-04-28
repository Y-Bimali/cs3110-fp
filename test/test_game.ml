(*open OUnit2 open Fp.Card open Fp.Tableau open Fp.Stockwaste open Fp.Foundation
  open Fp.Game

  let h1 = new_card Hearts 1 let h2 = new_card Hearts 2 let d1 = new_card
  Diamonds 1 let d8 = new_card Diamonds 8 let d11 = new_card Diamonds 11 let d12
  = new_card Diamonds 12 let d13 = new_card Diamonds 13 let c1 = new_card Clubs
  1 let c3 = new_card Clubs 3 let c7 = new_card Clubs 7 let c9 = new_card Clubs
  9 let c10 = new_card Clubs 10 let c11 = new_card Clubs 11 let s1 = new_card
  Spades 1 let s2 = new_card Spades 2 let s10 = new_card Spades 10

  (*Test case 1 with these properties: S to F: Can move Ace from Stock
  (stockwaste1) into Foundation (foundation1 or foundation2) ,S to T: Can move
  Ace from Stock (stockwaste2) into Tableau (tableau1), T to F: Can move Ace
  from Tableau (tableau1 - col 2) into Foundation (foundation1 or foundation2) ,
  T to T: Can move Ace from Tableau(tableau1 - col 2) to Tableau(tableau1 - col
  3), T to T: Can move 8,7 from Tableau (tableau 1- col 1) to Tableau(tableau1 -
  col 0), F to T: Can move 2 from Foundation (foundation2) to Tableau (tableau1
  - col 4) *)

  let foundation1 = put initialize h1 let foundation2 = put foundation1 h2 let
  stockwaste1 = add_sw [ s1; s2 ] empty_sw let stockwaste2 = add_sw [ d8 ]
  empty_sw

  let tableau1 = init_tab (List.flatten [ [ d1; d1; d1; d1; d1; d1; d1 ]; [ d1;
  d1; d1; d1; d1; d1 ]; [ c3; c3; c3; c3; c3 ]; [ h2; h2; h2; h2 ]; [ c1; c1; c1
  ]; [ c7; d8 ]; [ c9 ]; ])

  (*Test case 2 with these properties: S to F: Can not move King from Stock
  (stockwaste4) into Foundation(foundation1 or foundation2), S to T: Can not
  move Queen from Stock (stockwaste3) into Tableau (tableau2 - col 0), T to F:
  Can not move Jack from Tableau (tableau11 - col 2) into Foundation
  (foundation1 or foundation2),T to T: Can not move 10 from Tableau (tableau1-
  col 0) ) to Tableau(tableau1 - col 3) T to T: Can not move Jack,10 from
  Tableau (tableau1 - col 1) to Tableau (tableau1 - col 0) F to T: Can not move
  Ace from Foundation(foundation1) to Tableau(tableau1 - col 1) *)

  let stockwaste3 = add_sw [ d12; d1 ] empty_sw let stockwaste4 = add_sw [ d13 ]
  empty_sw

  let tableau2 = init_tab (List.flatten [ [ d1; d1; d1; d1; d1; d1; d1 ]; [ d1;
  d1; d1; d1; d1; d1 ]; [ c3; c3; c3; c3; c3 ]; [ h2; h2; h2; h2 ]; [ c11; c11;
  c11 ]; [ s10; d11 ]; [ c10 ]; ])

  (*let test_t_to_t _ = let g1 = game_from_parts foundation1 stockwaste3
  tableau2 in let g2 = game_from_parts foundation2 stockwaste1 tableau1 in
  assert_equal (t_to_t g1 "6" "6" "1") (g1, Some "Illegal Move."); assert_equal
  (t_to_t g2 "5" "6" "1") (g2, Some "Illegal Move.")*)

  let test_s_to_ft_works _ = let g1 = game_from_parts foundation1 stockwaste1
  tableau1 in let g2 = game_from_parts foundation2 stockwaste1 tableau1 in let
  g3 = game_from_parts foundation1 stockwaste2 tableau1 in assert_equal (snd
  (s_to_f g1)) None; assert_equal (snd (s_to_f g2)) None; assert_equal (snd
  (s_to_t g3 2)) None

  let test_s_to_ft_err _ = let g1 = game_from_parts foundation1 stockwaste4
  tableau2 in let g2 = game_from_parts foundation2 stockwaste4 tableau2 in let
  g3 = game_from_parts foundation1 stockwaste3 tableau2 in let g4 =
  game_from_parts foundation1 stockwaste3 tableau2 in assert_equal (snd (s_to_f
  g1)) (Some "This card cannot go in the foundation."); assert_equal (snd
  (s_to_f g2)) (Some "This card cannot go in the foundation."); assert_equal
  (snd (s_to_t g3 2)) (Some ("This card cannot go in column " ^ string_of_int 3
  ^ ". (Illegal Move)")); assert_equal (snd (s_to_t g4 20)) (Some (string_of_int
  20 ^ " is not a valid index in the tableau. Must be from 1 to 7."))

  (*let tab_tests = "Tableau only tests" >::: [ "test_t_to_t" >:: test_t_to_t
  ]*) let sw_tests1 = "Stockwaste tests where it is a valid move" >::: [
  "test_s_to_ft" >:: test_s_to_ft_works ]

  let sw_tests2 = "Stockwaste tests where it is not a valid move" >::: [
  "test_s_to_ft" >:: test_s_to_ft_err ]

  let () = (*run_test_tt_main tab_tests;*) run_test_tt_main sw_tests1;
  run_test_tt_main sw_tests2 *)
