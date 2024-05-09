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
    List.map to_string l1 @ BatList.make (List.length l2) "XXX"

  let to_col_lst col =
    let col = rep_ok col in
    let l1, l2 = BatList.takedrop col.vis col.cards in
    l1 @ BatList.make (List.length l2) (empty_card Spades)

  let empty_col = { cards = []; vis = 0 }
end

open Column

type t = c list
(* AF: [t] is a list of columns of cards [[c0;c1;...c6]].*)
(* RI: [List.length t] = 7.*)

exception InvalidColID
exception InvalidCardID
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

let rec find_match tab c2 c1vis n =
  if c1vis = [] then raise IllegalMove
  else
    match card_to_col tab c2 (List.hd c1vis) with
    | exception IllegalMove -> find_match tab c2 (List.tl c1vis) (n + 1)
    | _ -> n

let move_col_to_col tab c1 c2 =
  if c1 = c2 then tab
  else
    let tab = rep_ok tab in
    let col1 = get_col tab c1 in
    let i = find_match tab c2 (BatList.take col1.vis col1.cards) 0 in
    let tab1, cd_lst = pop_n_col_card tab c1 (i + 1) in
    put_n_col_card tab1 c2 (List.rev cd_lst)

let empty_tab = BatList.make 7 empty_col

let winnable tab =
  List.fold_left
    (fun a b -> a && b)
    true
    (List.map (fun a -> a.vis = List.length a.cards) tab)

let cheat_col_card tab col cardi =
  let tab = rep_ok tab in
  let col1 = get_col tab col in
  if List.length col1.cards < cardi || cardi < 1 then raise InvalidCardID
  else
    match BatList.at_opt col1.cards (List.length col1.cards - cardi) with
    | None -> raise InvalidCardID
    | Some x -> x

let to_str_lst tab = List.map col_str_lst tab

(* let to_str tab = List.fold_left (fun c d -> c ^ List.fold_left (fun a b -> a
   ^ " " ^ b) "" d ^ "\n") "" (to_str_lst tab) *)

let to_cd_lst tab = List.map to_col_lst tab
