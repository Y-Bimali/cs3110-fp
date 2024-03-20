open OUnit2
open Fp.Card

let test_string_of_suit _ =
  assert_equal "♥" (string_of_suit Hearts);
  assert_equal "♦" (string_of_suit Diamonds);
  assert_equal "♣" (string_of_suit Clubs);
  assert_equal "♠" (string_of_suit Spades)

let test_string_of_rank _ =
  assert_equal "A" (string_of_rank { rank = Ace; suit = Spades });
  assert_equal "J" (string_of_rank { rank = Jack; suit = Diamonds });
  assert_equal "Q" (string_of_rank { rank = Queen; suit = Clubs });
  assert_equal "K" (string_of_rank { rank = King; suit = Hearts });
  assert_equal "2" (string_of_rank { rank = Two; suit = Hearts });
  assert_equal "10" (string_of_rank { rank = Ten; suit = Diamonds })

let test_color_of _ =
  assert_equal Red (color_of { rank = Ace; suit = Hearts });
  assert_equal Black (color_of { rank = Two; suit = Spades });
  assert_equal Red (color_of { rank = Jack; suit = Diamonds });
  assert_equal Black (color_of { rank = Queen; suit = Clubs })

let test_to_string _ =
  assert_equal "A♥" (to_string { rank = Ace; suit = Hearts });
  assert_equal "K♦" (to_string { rank = King; suit = Diamonds });
  assert_equal "J♣" (to_string { rank = Jack; suit = Clubs });
  assert_equal "10♠" (to_string { rank = Ten; suit = Spades });
  assert_equal "2♠" (to_string { rank = Two; suit = Spades })

let test_empty_card _ =
  assert_equal { rank = Zero; suit = Spades } (empty_card Spades);
  assert_equal { rank = Zero; suit = Diamonds } (empty_card Diamonds);
  assert_equal { rank = Zero; suit = Clubs } (empty_card Clubs);
  assert_equal { rank = Zero; suit = Hearts } (empty_card Hearts)

let test_empty_card_two _ =
  let empty = empty_card Clubs in
  assert_equal Clubs (suit_of empty);
  assert_equal Zero (rank_of empty)

let test_num_of _ =
  assert_equal 0 (num_of { rank = Zero; suit = Spades });
  assert_equal 1 (num_of { rank = Ace; suit = Hearts });
  assert_equal 2 (num_of { rank = Two; suit = Hearts });
  assert_equal 3 (num_of { rank = Three; suit = Clubs });
  assert_equal 4 (num_of { rank = Four; suit = Spades });
  assert_equal 5 (num_of { rank = Five; suit = Hearts });
  assert_equal 6 (num_of { rank = Six; suit = Spades });
  assert_equal 7 (num_of { rank = Seven; suit = Hearts });
  assert_equal 8 (num_of { rank = Eight; suit = Hearts });
  assert_equal 9 (num_of { rank = Nine; suit = Diamonds });
  assert_equal 10 (num_of { rank = Ten; suit = Hearts });
  assert_equal 11 (num_of { rank = Jack; suit = Hearts });
  assert_equal 12 (num_of { rank = Queen; suit = Diamonds });
  assert_equal 13 (num_of { rank = King; suit = Clubs })

let tests =
  "test_tests"
  >::: [
         "test_string_of_suit" >:: test_string_of_suit;
         "test_string_of_rank" >:: test_string_of_rank;
         "test_color_of" >:: test_color_of;
         "test_to_string" >:: test_to_string;
         "test_empty_card" >:: test_empty_card;
         "test_num_of" >:: test_num_of;
         "test_string_of_rank" >:: test_string_of_rank;
         "test_empty_card_two" >:: test_empty_card_two;
       ]

let () = run_test_tt_main tests
