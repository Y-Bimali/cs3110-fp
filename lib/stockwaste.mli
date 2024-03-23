type t

exception EmptyWaste

val empty : t
(**[empty] is the empty stock and the empty waste*)

val draw : t -> t
(** [draw] sw is sw with the top card in stock removed and placed as the new top
    card in waste. If the stock is empty, it will put the cards from waste back
    into stock*)

val top : t -> Card.t
(**[top] is the top card in the waste pile. Raises EmptyWaste if the waste pile
   is empty.*)

val remove_top : t -> t
(** [remove_top sw] is [sw] with [top sw] removed and with a new top card.
    Raises EmptyWaste if the waste pile is empty*)
