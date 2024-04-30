open OUnit2
open Fp.Card
open Fp.Stockwaste
open Option

let test_empty_stockwaste _ = assert_equal (0, 0) (size_sw empty_sw)

let test_check_stock_empty _ =
  (*empty stock*)
  assert_equal true (check_stock_empty empty_sw);
  (*stock with one card*)
  assert_equal false (check_stock_empty (add_sw [ empty_card Clubs ] empty_sw));
  (*stock with multiple cards*)
  assert_equal false
    (check_stock_empty
       (add_sw
          [
            empty_card Clubs;
            empty_card Diamonds;
            empty_card Hearts;
            empty_card Spades;
          ]
          empty_sw))

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
  assert_equal None (top_sw empty_sw)

let test_top_sw_nonempty _ =
  (*one card in the waste*)
  let one_card = draw None (add_sw [ empty_card Clubs ] empty_sw) in
  assert_equal Clubs (suit_of (get (top_sw (get one_card))));
  (*multiple cards in the waste*)
  let many_cards =
    draw None
      (add_sw
         [
           empty_card Spades;
           empty_card Diamonds;
           empty_card Hearts;
           empty_card Clubs;
         ]
         empty_sw)
  in

  assert_equal Spades (suit_of (get (top_sw (get many_cards))));
  (*multiple identical cards in the waste*)
  let many_cards2 =
    draw None
      (add_sw
         [
           empty_card Hearts;
           empty_card Hearts;
           empty_card Hearts;
           empty_card Hearts;
         ]
         empty_sw)
  in
  assert_equal Hearts (suit_of (get (top_sw (get many_cards2))))

let test_draw _ =
  (*draw one card fgetm stock with one card*)
  let one_card = draw None (add_sw [ empty_card Clubs ] empty_sw) in
  assert_equal (0, 1) (size_sw (get one_card));
  (*draw multiple unique cards fgetm stock with multiple cards*)
  let many_cards =
    get
      (draw None
         (get
            (draw None
               (get
                  (draw None
                     (add_sw
                        [
                          empty_card Spades;
                          empty_card Diamonds;
                          empty_card Hearts;
                          empty_card Clubs;
                        ]
                        empty_sw))))))
  in
  assert_equal (1, 3) (size_sw many_cards);
  (*draw multiple unique cards fgetm stock with multiple cards*)
  let many_cards2 =
    get
      (draw None
         (get
            (draw None
               (add_sw
                  [
                    empty_card Spades;
                    empty_card Spades;
                    empty_card Spades;
                    empty_card Spades;
                  ]
                  empty_sw))))
  in
  assert_equal (2, 2) (size_sw many_cards2);
  (*draw as many cards as the stock has*)
  let many_cards3 =
    get
      (draw None
         (get
            (draw None
               (add_sw [ empty_card Spades; empty_card Diamonds ] empty_sw))))
  in
  assert_equal (0, 2) (size_sw many_cards3);

  (*draw more cards than the stack has - requires it to move cards back into
    stock*)
  let many_cards4 =
    draw None
      (get
         (draw None
            (get
               (draw None
                  (add_sw [ empty_card Hearts; empty_card Spades ] empty_sw)))))
  in
  assert_equal (2, 0) (size_sw (get many_cards4));

  (*draw more cards than the stack has and requires it to move cards back into
    stock, then continues to draw*)
  let many_cards4 =
    draw None
      (get
         (draw None
            (get
               (draw None
                  (get
                     (draw None
                        (add_sw
                           [ empty_card Hearts; empty_card Spades ]
                           empty_sw)))))))
  in

  assert_equal (1, 1) (size_sw (get many_cards4))

let test_remove_top_empty _ =
  (*nothing in the waste*)
  assert_equal None (remove_top empty_sw);
  (*removing more cards than there are in the waste*)
  let many_cards3 =
    draw None
      (get
         (draw None (add_sw [ empty_card Spades; empty_card Hearts ] empty_sw)))
  in
  assert_equal None
    (remove_top (get (remove_top (get (remove_top (get many_cards3))))))

let test_remove_top_nonempty _ =
  (*remove one card fgetm the waste when waste has one card*)
  let one_card = draw None (add_sw [ empty_card Spades ] empty_sw) in
  assert_equal (0, 0) (size_sw (get (remove_top (get one_card))));
  (*remove one card fgetm the waste with multiple cards*)
  let many_cards =
    draw None
      (get
         (draw None
            (get
               (draw None
                  (add_sw
                     [
                       empty_card Spades;
                       empty_card Diamonds;
                       empty_card Hearts;
                       empty_card Clubs;
                     ]
                     empty_sw)))))
  in
  assert_equal (1, 2) (size_sw (get (remove_top (get many_cards))));
  (*remove multiple cards fgetm the waste when the waste has multiple cards*)
  let many_cards2 =
    draw None
      (get
         (draw None
            (get
               (draw None
                  (add_sw
                     [
                       empty_card Spades;
                       empty_card Hearts;
                       empty_card Hearts;
                       empty_card Clubs;
                     ]
                     empty_sw)))))
  in
  assert_equal (1, 1)
    (size_sw (get (remove_top (get (remove_top (get many_cards2))))));
  (*remove all the cards fgetm the waste with multiple cards*)
  let many_cards3 =
    get
      (draw None
         (get
            (draw None
               (add_sw [ empty_card Spades; empty_card Hearts ] empty_sw))))
  in
  assert_equal (0, 0)
    (size_sw (get (remove_top (get (remove_top many_cards3)))))

let sw_tests =
  "test_tests"
  >::: [
         "test_empty_stockwaste" >:: test_empty_stockwaste;
         "test_add_sw" >:: test_add_sw;
         "test_top_sw_empty" >:: test_top_sw_empty;
         "test_top_sw_nonempty" >:: test_top_sw_nonempty;
         "test_draw" >:: test_draw;
         "test_remove_top_empty" >:: test_remove_top_empty;
         "test_remove_top_nonempty" >:: test_remove_top_nonempty;
         "test_check_stock_empty" >:: test_check_stock_empty;
       ]

let () = run_test_tt_main sw_tests
