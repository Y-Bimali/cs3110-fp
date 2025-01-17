open OUnit2
open Fp.Card
open Fp.Stockwaste
open Option

let four_card_deck =
  [
    empty_card Clubs; empty_card Diamonds; empty_card Hearts; empty_card Spades;
  ]

let rec draw_n n o sw =
  match n with
  | 0 -> sw
  | _ -> get (draw (fun _ -> o) (draw_n (n - 1) o sw))

let test_empty_stockwaste _ = assert_equal (0, 0) (size_sw empty_sw)

let test_check_stock_empty _ =
  (*empty stock*)
  assert_equal true (check_stock_empty empty_sw);
  (*stock with one card*)
  assert_equal false (check_stock_empty (add_sw [ empty_card Clubs ] empty_sw));
  (*stock with multiple cards*)
  assert_equal false (check_stock_empty (add_sw four_card_deck empty_sw))

let test_add_sw _ =
  (*adding nothing*)
  assert_equal (0, 0) (size_sw (add_sw [] empty_sw));
  (*adding one card*)
  assert_equal (1, 0) (size_sw (add_sw [ empty_card Clubs ] empty_sw));
  (*adding multiple cards*)
  assert_equal (4, 0) (size_sw (add_sw four_card_deck empty_sw))

let test_top_sw_empty _ =
  (*nothing in the waste*)
  assert_equal None (top_sw empty_sw)

let test_top_sw_nonempty _ =
  (*one card in the waste*)
  let one_card = draw (fun () -> None) (add_sw [ empty_card Clubs ] empty_sw) in
  assert_equal Clubs (suit_of (get (top_sw (get one_card))));
  (*multiple cards in the waste*)
  let many_cards = draw (fun () -> None) (add_sw four_card_deck empty_sw) in
  assert_equal Clubs (suit_of (get (top_sw (get many_cards))))

let test_draw_none _ =
  (*draw when the stock and waste are both empty*)
  assert_equal None (draw (fun () -> None) (add_sw [] empty_sw))

let test_draw_1_basic _ =
  (*draw one card from stock with one card- draw 1*)
  let one_card_1 =
    draw (fun () -> None) (add_sw [ empty_card Clubs ] empty_sw)
  in
  assert_equal (0, 1) (size_sw (get one_card_1));
  (*draw multiple unique cards from stock with multiple cards - draw 1*)
  let many_cards1 = draw_n 3 None (add_sw four_card_deck empty_sw) in
  (*draw multiple cards from stock with multiple cards- draw 1*)
  let many_cards5 = draw_n 2 None (add_sw four_card_deck empty_sw) in
  assert_equal (2, 2) (size_sw many_cards5);
  assert_equal (1, 3) (size_sw many_cards1)

let test_draw_1_redraw _ =
  (*draw as many cards as the stock has- draw 1*)
  let many_cards6 =
    draw_n 2 None (add_sw [ empty_card Spades; empty_card Diamonds ] empty_sw)
  in
  assert_equal (0, 2) (size_sw many_cards6);

  (*draw more cards than the stack has - requires it to move cards back into
    stock - draw 1*)
  let many_cards7 =
    draw
      (fun () -> None)
      (draw_n 2 None (add_sw [ empty_card Hearts; empty_card Spades ] empty_sw))
  in
  assert_equal (2, 0) (size_sw (get many_cards7));

  (*draw more cards than the stack has and requires it to move cards back into
    stock, then continues to draw- draw 1*)
  let many_cards8 =
    draw
      (fun () -> None)
      (draw_n 3 None (add_sw [ empty_card Hearts; empty_card Spades ] empty_sw))
  in
  assert_equal (1, 1) (size_sw (get many_cards8))

let test_draw_3_basic _ =
  (*draw one card from stock with one card- draw 3*)
  let one_card_2 =
    draw (fun () -> Some "3") (add_sw [ empty_card Hearts ] empty_sw)
  in
  assert_equal (0, 1) (size_sw (get one_card_2));
  (*draw two cards from stock with two cards in one draw- draw 3*)
  let two_card =
    draw
      (fun () -> Some "3")
      (add_sw [ empty_card Spades ] (add_sw [ empty_card Hearts ] empty_sw))
  in
  assert_equal (0, 2) (size_sw (get two_card));
  (*draw three cards from stock with three cards in one draw- draw 3*)
  let three_card =
    draw
      (fun () -> Some "3")
      (add_sw
         [ empty_card Diamonds ]
         (add_sw [ empty_card Spades ] (add_sw [ empty_card Hearts ] empty_sw)))
  in
  assert_equal (0, 3) (size_sw (get three_card))

let test_draw_3_redraw _ =
  (*draw multiple times cards from stock with multiple cards until stock is 0 -
    draw 3*)
  let many_cards2 =
    get
      (draw
         (fun () -> Some "3")
         (get (draw (fun () -> Some "3") (add_sw four_card_deck empty_sw))))
  in
  assert_equal (0, 4) (size_sw many_cards2);
  (*draw multiple times cards from stock with multiple cards until redraw stock-
    draw 3*)
  let many_cards3 = draw_n 3 (Some "3") (add_sw four_card_deck empty_sw) in
  assert_equal (4, 0) (size_sw many_cards3);
  (*draw multiple times cards from stock with multiple cards until redraw stock
    and continue to draw - draw 3*)
  let many_cards4 = draw_n 4 (Some "3") (add_sw four_card_deck empty_sw) in
  assert_equal (1, 3) (size_sw many_cards4)

let test_remove_top_empty _ =
  (*nothing in the waste*)
  assert_equal None (remove_top empty_sw);
  (*removing more cards than there are in the waste*)
  let many_cards3 =
    draw
      (fun () -> None)
      (get
         (draw
            (fun () -> None)
            (add_sw [ empty_card Spades; empty_card Hearts ] empty_sw)))
  in
  assert_equal None
    (remove_top (get (remove_top (get (remove_top (get many_cards3))))))

let test_remove_top_nonempty _ =
  (*remove one card from the waste when waste has one card*)
  let one_card =
    draw (fun () -> None) (add_sw [ empty_card Spades ] empty_sw)
  in
  assert_equal (0, 0) (size_sw (get (remove_top (get one_card))));
  (*remove one card from the waste with multiple cards*)
  let many_cards =
    draw
      (fun () -> None)
      (get
         (draw
            (fun () -> None)
            (get (draw (fun () -> None) (add_sw four_card_deck empty_sw)))))
  in
  assert_equal (1, 2) (size_sw (get (remove_top (get many_cards))));
  (*remove all the cards from the waste with multiple cards*)
  let many_cards3 =
    draw_n 2 None (add_sw [ empty_card Spades; empty_card Hearts ] empty_sw)
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
         "test_draw_none" >:: test_draw_none;
         "test_draw_1_basic" >:: test_draw_1_basic;
         "test_draw_1_redraw" >:: test_draw_1_redraw;
         "test_draw_3" >:: test_draw_3_basic;
         "test_draw_3" >:: test_draw_3_redraw;
         "test_remove_top_empty" >:: test_remove_top_empty;
         "test_remove_top_nonempty" >:: test_remove_top_nonempty;
         "test_check_stock_empty" >:: test_check_stock_empty;
       ]

let () = run_test_tt_main sw_tests
