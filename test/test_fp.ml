open OUnit2
open Fp.Card
open Fp.Tableau
open Fp.Stockwaste
open BatList

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

let test_new_card _ =
  assert_equal "0♠" (to_string (new_card Spades 0));
  assert_equal "A♠" (to_string (new_card Spades 1));
  assert_equal "2♠" (to_string (new_card Spades 2));
  assert_equal "3♠" (to_string (new_card Spades 3));
  assert_equal "4♠" (to_string (new_card Spades 4));
  assert_equal "5♠" (to_string (new_card Spades 5));
  assert_equal "6♠" (to_string (new_card Spades 6));
  assert_equal "7♠" (to_string (new_card Spades 7));
  assert_equal "8♠" (to_string (new_card Spades 8));
  assert_equal "9♠" (to_string (new_card Spades 9));
  assert_equal "10♠" (to_string (new_card Spades 10));
  assert_equal "J♠" (to_string (new_card Spades 11));
  assert_equal "Q♠" (to_string (new_card Spades 12));
  assert_equal "K♠" (to_string (new_card Spades 13));
  assert_raises UnusableRank (fun () -> new_card Spades 14)

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

let t1 =
  let x = { rank = Seven; suit = Hearts } in
  init_tab (BatList.make 28 x)

let t2 =
  let x = { rank = Ace; suit = Hearts } in
  let y = { rank = Ten; suit = Diamonds } in
  let z = { rank = Queen; suit = Spades } in
  init_tab (y :: List.flatten (BatList.make 9 [ x; y; z ]))

let t3 =
  let x = { rank = King; suit = Diamonds } in
  let y = { rank = Queen; suit = Spades } in
  let z = { rank = Jack; suit = Hearts } in
  init_tab (y :: List.flatten (BatList.make 9 [ x; y; z ]))

let test_init_tab _ =
  assert_equal (to_str_lst t1)
    (let x = " 7♥" in
     [
       [ x ];
       [ x; "XXX" ];
       [ x; "XXX"; "XXX" ];
       [ x; "XXX"; "XXX"; "XXX" ];
       [ x; "XXX"; "XXX"; "XXX"; "XXX" ];
       [ x; "XXX"; "XXX"; "XXX"; "XXX"; "XXX" ];
       [ x; "XXX"; "XXX"; "XXX"; "XXX"; "XXX"; "XXX" ];
     ]);
  assert_equal (to_str_lst t2)
    (let x = " A♥" in
     let y = "10♦" in
     let z = " Q♠" in
     [
       [ z ];
       [ x; "XXX" ];
       [ x; "XXX"; "XXX" ];
       [ z; "XXX"; "XXX"; "XXX" ];
       [ x; "XXX"; "XXX"; "XXX"; "XXX" ];
       [ x; "XXX"; "XXX"; "XXX"; "XXX"; "XXX" ];
       [ y; "XXX"; "XXX"; "XXX"; "XXX"; "XXX"; "XXX" ];
     ])

let test_peekpop_tab _ =
  assert_equal
    (List.map (peek_col_card t1) [ 0; 3; 6 ])
    (let x = { rank = Seven; suit = Hearts } in
     [ Some x; Some x; Some x ]);
  assert_equal
    (List.map2
       (fun (a, b) c ->
         ( (match peek_col_card a c with
           | None -> ""
           | Some x -> to_string x),
           to_string b ))
       (List.map (pop_col_card t2) [ 0; 4; 6 ])
       [ 0; 4; 6 ])
    [ ("", "Q♠"); ("10♦", "A♥"); ("A♥", "10♦") ]

let test_cardmoves_tab _ =
  assert_equal
    (let y = { rank = Queen; suit = Spades } in
     let z = { rank = Jack; suit = Hearts } in
     to_str_lst (card_to_col (card_to_col t3 5 y) 5 z))
    (let x = " K♦" in
     let y = " Q♠" in
     let z = " J♥" in
     [
       [ z ];
       [ x; "XXX" ];
       [ x; "XXX"; "XXX" ];
       [ z; "XXX"; "XXX"; "XXX" ];
       [ x; "XXX"; "XXX"; "XXX"; "XXX" ];
       [ z; y; x; "XXX"; "XXX"; "XXX"; "XXX"; "XXX" ];
       [ y; "XXX"; "XXX"; "XXX"; "XXX"; "XXX"; "XXX" ];
     ]);
  assert_equal
    (to_str_lst
       (move_col_to_col
          (move_col_to_col (move_col_to_col t3 0 6 1) 6 5 2)
          5 0 3))
    (let x = " K♦" in
     let y = " Q♠" in
     let z = " J♥" in
     [
       [ z; y; x ];
       [ x; "XXX" ];
       [ x; "XXX"; "XXX" ];
       [ z; "XXX"; "XXX"; "XXX" ];
       [ x; "XXX"; "XXX"; "XXX"; "XXX" ];
       [ y; "XXX"; "XXX"; "XXX"; "XXX" ];
       [ x; "XXX"; "XXX"; "XXX"; "XXX"; "XXX" ];
     ])

let tests =
  "test_tests"
  >::: [
         "test_string_of_suit" >:: test_string_of_suit;
         "test_string_of_rank" >:: test_string_of_rank;
         "test_color_of" >:: test_color_of;
         "test_to_string" >:: test_to_string;
         "test_empty_card" >:: test_empty_card;
         "test_num_of" >:: test_num_of;
         "test_new_card" >:: test_new_card;
         "test_string_of_rank" >:: test_string_of_rank;
         "test_empty_card_two" >:: test_empty_card_two;
         "test_empty_stockwaste" >:: test_empty_stockwaste;
         "test_add_sw" >:: test_add_sw;
         "test_top_sw_empty" >:: test_top_sw_empty;
         "test_top_sw_nonempty" >:: test_top_sw_nonempty;
         "test_draw" >:: test_draw;
       ]

let tab_tests =
  "tab_tests"
  >::: [
         "test_init_tab" >:: test_init_tab;
         "test_peekpop_tab" >:: test_peekpop_tab;
         "test_cardmoves_tab" >:: test_cardmoves_tab;
       ]

let () = run_test_tt_main tests
let () = run_test_tt_main tab_tests
