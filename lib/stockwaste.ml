type t = {
  stock : Card.t list;
  waste : Card.t list;
}

let empty_sw = { stock = []; waste = [] }
let add_sw cards sw = { stock = cards @ sw.stock; waste = sw.waste }
let size_sw sw = (List.length sw.stock, List.length sw.waste)

exception EmptyWaste

let redraw sw = { stock = List.rev sw.waste; waste = sw.stock }

let draw sw =
  if List.is_empty sw.stock then redraw sw
  else { stock = List.tl sw.stock; waste = List.hd sw.stock :: sw.waste }

let top_sw sw =
  match sw.waste with
  | [] -> raise EmptyWaste
  | h :: _ -> h

let remove_top sw =
  if sw.waste = [] then raise EmptyWaste
  else { stock = sw.stock; waste = List.tl sw.waste }
