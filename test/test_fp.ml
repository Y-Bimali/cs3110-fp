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

let suite =
  "test_suite"
  >::: [
         "test_string_of_suit" >:: test_string_of_suit;
         "test_string_of_rank" >:: test_string_of_rank;
         "test_color_of" >:: test_color_of;
         "test_to_string" >:: test_to_string;
         "test_empty_card" >:: test_empty_card;
       ]

let () = run_test_tt_main suite
