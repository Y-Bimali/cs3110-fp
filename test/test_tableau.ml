open OUnit2
open Fp.Card
open Fp.Tableau
open BatList

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

let t4 =
  let x = new_card Hearts 1 in
  let y = new_card Spades 2 in
  let z = new_card Diamonds 3 in
  init_tab (y :: List.flatten (BatList.make 9 [ x; y; z ]))

let test_init_tab _ =
  assert_equal (to_str_lst t1)
    (let x = "7♥" in
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
    (let x = "A♥" in
     let y = "10♦" in
     let z = "Q♠" in
     [
       [ z ];
       [ x; "XXX" ];
       [ x; "XXX"; "XXX" ];
       [ z; "XXX"; "XXX"; "XXX" ];
       [ x; "XXX"; "XXX"; "XXX"; "XXX" ];
       [ x; "XXX"; "XXX"; "XXX"; "XXX"; "XXX" ];
       [ y; "XXX"; "XXX"; "XXX"; "XXX"; "XXX"; "XXX" ];
     ]);
  assert_raises (Failure "Not 28 cards") (fun () ->
      init_tab (BatList.make 27 (new_card Spades 2)))

let test_peek_tab _ =
  assert_equal
    (List.map (peek_col_card t1) [ 0; 3; 6 ])
    (let x = { rank = Seven; suit = Hearts } in
     [ Some x; Some x; Some x ]);
  assert_raises InvalidColID (fun _ -> peek_col_card t1 ~-6)

let test_empty_tab _ =
  assert_equal (to_str_lst empty_tab) [ []; []; []; []; []; []; [] ];
  assert_equal (peek_col_card empty_tab 0) None

let test_pop_tab _ =
  assert_equal
    (List.map2
       (fun (a, b) c ->
         ( (match peek_col_card a c with
           | None -> ""
           | Some x -> to_string x),
           to_string b ))
       (List.map (pop_col_card t2) [ 0; 4; 6 ])
       [ 0; 4; 6 ])
    [ ("", "Q♠"); ("10♦", "A♥"); ("A♥", "10♦") ];
  assert_raises EmptyCol (fun _ -> pop_col_card (fst (pop_col_card t1 0)) 0);
  assert_raises InvalidColID (fun _ -> pop_col_card (fst (pop_col_card t1 9)) 0)

let test_card_to_col _ =
  assert_equal
    (let y = { rank = Queen; suit = Spades } in
     let z = { rank = Jack; suit = Hearts } in
     to_str_lst (card_to_col (card_to_col t3 5 y) 5 z))
    (let x = "K♦" in
     let y = "Q♠" in
     let z = "J♥" in
     [
       [ z ];
       [ x; "XXX" ];
       [ x; "XXX"; "XXX" ];
       [ z; "XXX"; "XXX"; "XXX" ];
       [ x; "XXX"; "XXX"; "XXX"; "XXX" ];
       [ z; y; x; "XXX"; "XXX"; "XXX"; "XXX"; "XXX" ];
       [ y; "XXX"; "XXX"; "XXX"; "XXX"; "XXX"; "XXX" ];
     ]);
  assert_raises IllegalMove (fun _ -> card_to_col t2 0 (new_card Hearts 0));
  assert_raises InvalidColID (fun _ -> card_to_col t2 10 (new_card Hearts 0))

let test_col_to_col_tab _ =
  assert_equal
    (to_str_lst
       (move_col_to_col (move_col_to_col (move_col_to_col t3 0 6) 6 5) 5 0))
    (let x = "K♦" in
     let y = "Q♠" in
     let z = "J♥" in
     [
       [ z; y; x ];
       [ x; "XXX" ];
       [ x; "XXX"; "XXX" ];
       [ z; "XXX"; "XXX"; "XXX" ];
       [ x; "XXX"; "XXX"; "XXX"; "XXX" ];
       [ y; "XXX"; "XXX"; "XXX"; "XXX" ];
       [ x; "XXX"; "XXX"; "XXX"; "XXX"; "XXX" ];
     ]);
  assert_equal
    (to_str_lst
       (let a, b = pop_col_card t4 1 in
        card_to_col a 1 b))
    (let x = "A♥" in
     let y = "2♠" in
     let z = "3♦" in
     [
       [ z ];
       [ x; y ];
       [ x; "XXX"; "XXX" ];
       [ z; "XXX"; "XXX"; "XXX" ];
       [ x; "XXX"; "XXX"; "XXX"; "XXX" ];
       [ x; "XXX"; "XXX"; "XXX"; "XXX"; "XXX" ];
       [ y; "XXX"; "XXX"; "XXX"; "XXX"; "XXX"; "XXX" ];
     ]);
  assert_equal (move_col_to_col t3 5 5) t3

let test_invalid_col_to_col_tab _ =
  assert_raises IllegalMove (fun _ -> move_col_to_col t3 2 6);
  assert_raises IllegalMove (fun _ ->
      card_to_col (move_col_to_col t3 0 6) 0 (new_card Hearts 0));
  assert_raises IllegalMove (fun _ -> move_col_to_col t2 6 1)

let test_to_cd_lst _ =
  assert_equal (to_cd_lst t1)
    (let z = new_card Hearts 7 in
     let a = empty_card Spades in
     [
       [ z ];
       [ z; a ];
       [ z; a; a ];
       [ z; a; a; a ];
       [ z; a; a; a; a ];
       [ z; a; a; a; a; a ];
       [ z; a; a; a; a; a; a ];
     ])

let test_cheat _ =
  assert_equal (cheat_col_card t3 2 2) { rank = Queen; suit = Spades };
  assert_equal (cheat_col_card t3 6 1) { rank = Jack; suit = Hearts };
  assert_equal (cheat_col_card t3 0 1) { rank = Jack; suit = Hearts }

let test_invalid_cheat _ =
  assert_raises InvalidCardID (fun _ -> cheat_col_card t3 0 0);
  assert_raises InvalidCardID (fun _ -> cheat_col_card t3 0 5);
  assert_raises InvalidColID (fun _ -> cheat_col_card t3 7 1)

let test_winnable _ =
  assert_equal (winnable t3) false;
  assert_equal (winnable empty_tab) true;
  assert_equal
    (winnable (card_to_col empty_tab 0 { rank = King; suit = Diamonds }))
    true

let test_lowest_col_index _ =
  assert_equal (lowest_col_index t2) (Some 1);
  assert_equal (lowest_col_index t3) (Some 0)

let tab_tests =
  "tab_tests"
  >::: [
         "test_init_tab" >:: test_init_tab;
         "test_peek_tab" >:: test_peek_tab;
         "test_empty_tab" >:: test_empty_tab;
         "test_pop_tab" >:: test_pop_tab;
         "test_card_to_col" >:: test_card_to_col;
         "test_col_to_col_tab" >:: test_col_to_col_tab;
         "test_invalid_col_to_col_tab" >:: test_invalid_col_to_col_tab;
         "test_to_cd_lst" >:: test_to_cd_lst;
         "test_cheat" >:: test_cheat;
         "test_invalid_cheat" >:: test_invalid_cheat;
         "test_winnable" >:: test_winnable;
         "test_lowest_col_index" >:: test_lowest_col_index;
       ]

let () = run_test_tt_main tab_tests
