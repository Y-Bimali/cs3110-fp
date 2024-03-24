open OUnit2
open Fp.Card
open Fp.Stockwaste

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

let test_empty_stockwaste _ = assert_equal (0, 0) (size_sw empty_sw)

let test_add_sw _ =
  (*adding nothing*)
  assert_equal (0, 0) (size_sw (add_sw [] empty_sw));
  (*adding one card*)
  assert_equal (1, 0) (size_sw (add_sw [ empty_card Clubs ] empty_sw));
  (*adding multiple cards*)
  assert_equal (4, 0)
    (size_sw
       (add_sw
          [
            empty_card Clubs;
            empty_card Diamonds;
            empty_card Hearts;
            empty_card Spades;
          ]
          empty_sw))

let test_top_sw_empty _ =
  (*nothing in the waste*)
  assert_raises EmptyWaste (fun () -> top_sw empty_sw)

let test_top_sw_nonempty _ =
  (*one card in the waste*)
  let one_card = draw (add_sw [ empty_card Clubs ] empty_sw) in
  assert_equal Clubs (suit_of (top_sw one_card));
  (*multiple cards in the waste*)
  let many_cards =
    draw
      (add_sw
         [
           empty_card Spades;
           empty_card Diamonds;
           empty_card Hearts;
           empty_card Clubs;
         ]
         empty_sw)
  in
  assert_equal Spades (suit_of (top_sw many_cards));
  (*multiple identical cards in the waste*)
  let many_cards2 =
    draw
      (add_sw
         [
           empty_card Hearts;
           empty_card Hearts;
           empty_card Hearts;
           empty_card Hearts;
         ]
         empty_sw)
  in
  assert_equal Hearts (suit_of (top_sw many_cards2))

let test_draw _ =
  (*draw one card from stock with one card*)
  let one_card = draw (add_sw [ empty_card Clubs ] empty_sw) in
  assert_equal (0, 1) (size_sw one_card);
  (*draw multiple unique cards from stock with multiple cards*)
  let many_cards =
    draw
      (draw
         (draw
            (add_sw
               [
                 empty_card Spades;
                 empty_card Diamonds;
                 empty_card Hearts;
                 empty_card Clubs;
               ]
               empty_sw)))
  in
  assert_equal (1, 3) (size_sw many_cards);
  (*draw multiple unique cards from stock with multiple cards*)
  let many_cards2 =
    draw
      (draw
         (add_sw
            [
              empty_card Spades;
              empty_card Spades;
              empty_card Spades;
              empty_card Spades;
            ]
            empty_sw))
  in
  assert_equal (2, 2) (size_sw many_cards2);
  (*draw as many cards as the stock has*)
  let many_cards3 =
    draw (draw (add_sw [ empty_card Spades; empty_card Diamonds ] empty_sw))
  in
  assert_equal (0, 2) (size_sw many_cards3);

  (*draw more cards than the stack has - requires it to move cards back into
    stock*)
  let many_cards4 =
    draw
      (draw (draw (add_sw [ empty_card Hearts; empty_card Spades ] empty_sw)))
  in
  assert_equal (2, 0) (size_sw many_cards4);

  (*draw more cards than the stack has and requires it to move cards back into
    stock, then continues to draw*)
  let many_cards4 =
    draw
      (draw
         (draw
            (draw (add_sw [ empty_card Hearts; empty_card Spades ] empty_sw))))
  in
  assert_equal (1, 1) (size_sw many_cards4)

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
         "test_empty_stockwaste" >:: test_empty_stockwaste;
         "test_add_sw" >:: test_add_sw;
         "test_top_sw_empty" >:: test_top_sw_empty;
         "test_top_sw_nonempty" >:: test_top_sw_nonempty;
         "test_draw" >:: test_draw;
       ]

let () = run_test_tt_main tests
