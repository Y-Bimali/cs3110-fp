open OUnit2
(** Be careful about what you uncomment, otherwise will not compile. *)

open Fp.Card
open Fp.Tableau
open Fp.Stockwaste
open Fp.Foundation
open Fp.Game

let h1 = new_card Hearts 1
let h2 = new_card Hearts 2

(* let h3 = new_card Hearts 3 let h4 = new_card Hearts 4 let h5 = new_card
   Hearts 5 let h6 = new_card Hearts 6 let h7 = new_card Hearts 7 let h8 =
   new_card Hearts 8 let h9 = new_card Hearts 9 let h10 = new_card Hearts 10 let
   h11 = new_card Hearts 11 let h12 = new_card Hearts 12 let h13 = new_card
   Hearts 13 *)
let d1 = new_card Diamonds 1

(* let d2 = new_card Diamonds 2 let d3 = new_card Diamonds 3 let d4 = new_card
   Diamonds 4 let d5 = new_card Diamonds 5 let d6 = new_card Diamonds 6 let d7 =
   new_card Diamonds 7 *)
let d8 = new_card Diamonds 8

(* let d9 = new_card Diamonds 9 let d10 = new_card Diamonds 10 *)
let d11 = new_card Diamonds 11
let d12 = new_card Diamonds 12

(* let d13 = new_card Diamonds 13 *)
let c1 = new_card Clubs 1

(* let c2 = new_card Clubs 2 *)
let c3 = new_card Clubs 3

(* let c4 = new_card Clubs 4 let c5 = new_card Clubs 5 let c6 = new_card Clubs
   6 *)
let c7 = new_card Clubs 7

(* let c8 = new_card Clubs 8 *)
let c9 = new_card Clubs 9
let c10 = new_card Clubs 10
let c11 = new_card Clubs 11

(* let c12 = new_card Clubs 12 let c13 = new_card Clubs 13 *)
let s1 = new_card Spades 1
let s2 = new_card Spades 2

(* let s3 = new_card Spades 3 let s4 = new_card Spades 4 let s5 = new_card
   Spades 5 let s6 = new_card Spades 6 let s7 = new_card Spades 7 let s8 =
   new_card Spades 8 let s9 = new_card Spades 9 *)
let s10 = new_card Spades 10
(* let s11 = new_card Spades 11 let s12 = new_card Spades 12 let s13 = new_card
   Spades 13 *)

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
let stockwaste1 = add_sw [ s1; s2 ] empty_sw
(*let stockwaste2 = add_sw [ d8 ] empty_sw*)

let tableau1 =
  init_tab
    (List.flatten
       [
         [ c9; d8; c7; c1; c1; c1; h2 ];
         [ h2; h2; h2; c3; c3; c3 ];
         [ c3; c3; d1; d1; d1 ];
         [ d1; d1; d1; d1 ];
         [ d1; d1; d1 ];
         [ d1; d1 ];
         [ d1 ];
       ])

(*Test case 2 with these properties: S to F: Can not move King from Stock
  (stockwaste4) into Foundation(foundation1 or foundation2), S to T: Can not
  move Queen from Stock (stockwaste3) into Tableau (tableau1 - col 0), T to F:
  Can not move Jack from Tableau (tableau11 - col 2) into Foundation
  (foundation1 or foundation2),T to T: Can not move 10 from Tableau (tableau1-
  col 0) ) to Tableau(tableau1 - col 3) T to T: Can not move Jack,10 from
  Tableau (tableau1 - col 1) to Tableau (tableau1 - col 0) F to T: Can not move
  Ace from Foundation(foundation1) to Tableau(tableau1 - col 1) *)
(* let foundation1 = put initialize h1 let foundation2 = put foundation1 h2 *)
let stockwaste3 = add_sw [ d12; d1 ] empty_sw
(* let stockwaste4 = add_sw [ d13 ] empty_sw *)

let tableau2 =
  init_tab
    [
      c10;
      d11;
      s10;
      c11;
      c11;
      c11;
      h2;
      h2;
      h2;
      h2;
      c3;
      c3;
      c3;
      c3;
      c3;
      d1;
      d1;
      d1;
      d1;
      d1;
      d1;
      d1;
      d1;
      d1;
      d1;
      d1;
      d1;
      d1;
    ]

let test_t_to_t _ =
  let g1 = game_from_parts foundation1 stockwaste3 tableau2 in
  let g2 = game_from_parts foundation2 stockwaste1 tableau1 in
  assert_equal (t_to_t g1 "6" "6" "1") (g1, Some "Illegal Move.");
  assert_equal (t_to_t g2 "5" "6" "1") (g2, Some "Illegal Move.")

let tab_tests = "Tableau only tests" >::: [ "test_t_to_t" >:: test_t_to_t ]
let () = run_test_tt_main tab_tests
