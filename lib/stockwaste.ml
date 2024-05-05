type t = {
  stock : Card.t list;
  waste : Card.t list;
}

(**[check_three three_opt] is a helper function that is a bool representing if
   the user wants to draw 3*)
let check_three three_opt =
  match three_opt with
  | Some _ -> true
  | None -> false

let get_stock_3 (sw : t) =
  match List.length sw.stock with
  | 2 -> List.tl (List.tl sw.stock)
  | 1 -> List.tl sw.stock
  | _ -> List.tl (List.tl (List.tl sw.stock))

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
  else if check_three (three_opt ()) then
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
