open Card
open BatList

module Column = struct
  type c = {
    cards : Card.t list;
    vis : int;
  }
  (* AF: [c.cards] is a list [[c0;c1;...c6]] of columns of cards where the cards
     are listed from bottom to top. [c.vis] is how many cards are visible.*)
  (* RI: The bottom [c.vis] cards must be a compatible stack. If [c.cards = []],
     then [c.vis = 0]. Otherwise, 1 <= [t.vis] <= [List.length c.cards].*)

  (* [comp_stack lst] checks if the list of cards [lst], ordered from bottom to
     top, is a valid stack.*)

  let rec comp_stack lst =
    match lst with
    | [] -> true
    | _ :: [] -> true
    | h1 :: h2 :: _ ->
        if num_of h2 - num_of h1 = 1 && color_of h2 <> color_of h1 then
          comp_stack (tl lst)
        else false

  let rep_ok col =
    match col.cards with
    | [] -> if col.vis = 0 then col else failwith "RI"
    | _ ->
        if 1 > col.vis || col.vis > List.length col.cards then failwith "RI"
        else if comp_stack (BatList.take col.vis col.cards) then col
        else failwith "RI"

  let init_col lst = { cards = lst; vis = 1 }

  let col_str_lst col =
    let col = rep_ok col in
    let l1, l2 = BatList.takedrop col.vis col.cards in
    List.map
      (fun a -> if rank_of a = Ten then to_string a else " " ^ to_string a)
      l1
    @ BatList.make (List.length l2) "XXX"
end

open Column

type t = c list
(* AF: [t] is a list of columns of cards [[c0;c1;...c6]].*)
(* RI: [List.length t] = 7.*)

exception InvalidColID
exception IllegalMove
exception EmptyCol

let rep_ok tab : t =
  let tab = List.map Column.rep_ok tab in
  if List.length tab = 7 then tab else failwith "RI"

let get_col tab id =
  if id < 0 || id > 6 then raise InvalidColID else List.nth tab id

let rec construct n lst : t =
  match n with
  | 1 -> init_col lst :: []
  | _ ->
      let l1, l2 = BatList.takedrop n lst in
      init_col l1 :: construct (n - 1) l2

let init_tab lst : t =
  if List.length lst <> 28 then failwith "Not 28 cards"
  else rep_ok (List.rev (construct 7 lst))

let peek_col_card tab col =
  let colv = get_col tab col in
  match colv.cards with
  | [] -> None
  | _ -> Some (hd colv.cards)

let pop_col_card tab col =
  let colv = get_col (rep_ok tab) col in
  match colv.cards with
  | [] -> raise EmptyCol
  | h :: tl ->
      ( rep_ok
          (BatList.modify_at col
             (fun _ ->
               {
                 cards = tl;
                 vis =
                   (if List.length tl = 0 then 0
                    else if colv.vis = 1 then 1
                    else colv.vis - 1);
               })
             tab),
        h )

let card_to_col tab col cd =
  let colv = get_col (rep_ok tab) col in
  if
    comp_stack (cd :: BatList.take colv.vis colv.cards)
    && (colv.vis <> 0 || rank_of cd = King)
  then
    rep_ok
      (BatList.modify_at col
         (fun _ -> { cards = cd :: colv.cards; vis = colv.vis + 1 })
         tab)
  else raise IllegalMove

let rec pop_n_col_card tab col n =
  match n with
  | 0 -> (tab, [])
  | _ ->
      let old_tab, cd = pop_col_card tab col in
      let new_tab, cd_lst = pop_n_col_card old_tab col (n - 1) in
      (rep_ok new_tab, cd :: cd_lst)

let rec put_n_col_card tab col cd_lst =
  match cd_lst with
  | [] -> tab
  | h :: t -> rep_ok (put_n_col_card (card_to_col tab col h) col t)

let move_col_to_col tab c1 c2 i =
  let tab = rep_ok tab in
  let col1 = get_col tab c1 in
  if i > col1.vis || i < 0 then raise IllegalMove
  else
    let tab1, cd_lst = pop_n_col_card tab c1 i in
    put_n_col_card tab1 c2 (List.rev cd_lst)

let to_str_lst tab = List.map col_str_lst tab
