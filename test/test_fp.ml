open OUnit2
open Fp.Card
open Fp.Tableau
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

let card_tests =
  "card_tests"
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

let tab_tests =
  "tab_tests"
  >::: [
         "test_init_tab" >:: test_init_tab;
         "test_peekpop_tab" >:: test_peekpop_tab;
         "test_cardmoves_tab" >:: test_cardmoves_tab;
       ]

let () = run_test_tt_main card_tests
let () = run_test_tt_main tab_tests
