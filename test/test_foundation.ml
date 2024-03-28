open OUnit2
open Fp.Card
open Fp.Foundation

let f1 =
  set (new_card Spades 10) (new_card Hearts 12) (new_card Clubs 4)
    (new_card Diamonds 9)

let f2 =
  set (new_card Spades 9) (new_card Hearts 12) (new_card Clubs 4)
    (new_card Diamonds 9)

let f3 =
  set (new_card Spades 10) (new_card Hearts 11) (new_card Clubs 4)
    (new_card Diamonds 9)

let f4 =
  set (new_card Spades 10) (new_card Hearts 12) (new_card Clubs 3)
    (new_card Diamonds 9)

let f5 =
  set (new_card Spades 10) (new_card Hearts 12) (new_card Clubs 4)
    (new_card Diamonds 8)

let f6 =
  set (new_card Spades 13) (new_card Hearts 13) (new_card Clubs 13)
    (new_card Diamonds 13)

let test_valid_move _ =
  assert_equal true (valid_move f1 (new_card Spades 11));
  assert_equal true (valid_move f1 (new_card Hearts 13));
  assert_equal true (valid_move f1 (new_card Clubs 5));
  assert_equal true (valid_move f1 (new_card Diamonds 10));
  assert_equal false (valid_move f1 (new_card Diamonds 12));
  assert_equal false (valid_move f1 (new_card Spades 4))

let test_put _ =
  assert_equal f1 (put f2 (new_card Spades 10));
  assert_equal f1 (put f3 (new_card Hearts 12));
  assert_equal f1 (put f4 (new_card Clubs 4));
  assert_equal f1 (put f5 (new_card Diamonds 9));
  assert_raises InvalidMove (fun () -> put f1 (new_card Spades 6))

let test_top_cards _ =
  assert_equal
    [
      new_card Spades 10;
      new_card Hearts 12;
      new_card Clubs 4;
      new_card Diamonds 9;
    ]
    (top_cards f1)

let test_is_complete _ =
  assert_equal false (is_complete f1);
  assert_equal true (is_complete f6)

let test_remove _ =
  assert_equal f2 (remove f1 (new_card Spades 10));
  assert_equal f3 (remove f1 (new_card Hearts 12));
  assert_equal f4 (remove f1 (new_card Clubs 4));
  assert_equal f5 (remove f1 (new_card Diamonds 9));
  assert_raises InvalidMove (fun () -> remove f1 (new_card Diamonds 3));
  assert_raises InvalidMove (fun () -> remove initialize (empty_card Diamonds))

let test_set _ =
  assert_equal initialize
    (set (empty_card Spades) (empty_card Hearts) (empty_card Clubs)
       (empty_card Diamonds));
  assert_raises (Failure "RI") (fun () ->
      set (empty_card Spades) (empty_card Spades) (empty_card Clubs)
        (empty_card Diamonds))
(* This tests the rep_ok failure.*)

let tests =
  "test_tests"
  >::: [
         "test_valid_move" >:: test_valid_move;
         "test_put" >:: test_put;
         "test_top_cards" >:: test_top_cards;
         "test_is_complete" >:: test_is_complete;
         "test_remove" >:: test_remove;
         "test_set" >:: test_set;
       ]

let () = run_test_tt_main tests
