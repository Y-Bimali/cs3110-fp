(* AF: The stockwaste (t) is a record of the form: { stock : Card.t list; waste
   : Card.t list } where the first entry is a list of Cards representing the
   stock, and the second entry is a a list of Cards representing the waste.*)
(* RI: None.*)

type t = {
  stock : Card.t list;
  waste : Card.t list;
}

(**[get_stock_3 sw] is the result of drawing from the stock when there is less
   than 3 cards remaining.*)
let get_stock_3 (sw : t) =
  match List.length sw.stock with
  | 2 -> []
  | 1 -> []
  | _ -> List.tl (List.tl (List.tl sw.stock))

(**[get_waste_3 sw] is the result of drawing from the stock when there is less
   than 3 cards remaining.*)
let get_waste_3 (sw : t) =
  match List.length sw.stock with
  | 2 -> List.nth sw.stock 1 :: List.nth sw.stock 0 :: sw.waste
  | 1 -> List.nth sw.stock 0 :: sw.waste
  | _ ->
      List.nth sw.stock 2 :: List.nth sw.stock 1 :: List.nth sw.stock 0
      :: sw.waste

let empty_sw = { stock = []; waste = [] }
let add_sw cards sw = { stock = cards @ sw.stock; waste = sw.waste }
let size_sw sw = (List.length sw.stock, List.length sw.waste)
let redraw sw = { stock = List.rev sw.waste; waste = sw.stock }

let draw three_opt sw =
  if List.is_empty sw.stock && List.is_empty sw.waste then None
  else if List.is_empty sw.stock then Some (redraw sw)
  else if three_opt () <> None then
    Some { stock = get_stock_3 sw; waste = get_waste_3 sw }
  else Some { stock = List.tl sw.stock; waste = List.hd sw.stock :: sw.waste }

let top_sw sw =
  match sw.waste with
  | [] -> None
  | h :: _ -> Some h

let remove_top sw =
  if sw.waste = [] then None
  else Some { stock = sw.stock; waste = List.tl sw.waste }

let check_stock_empty sw = sw.stock = []
let get_waste sw = sw.waste
