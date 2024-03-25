type t

exception EmptyWaste

val empty_sw : t
(**[empty_sw] is the empty stock and the empty waste*)

val add_sw : Card.t list -> t -> t
(**[add_sw] adds a card list to the stock*)

val size_sw : t -> int * int
(**[size_sw] is a pair where the first term is the size of the stock and the
   second term is the size of the waste.*)

val draw : t -> t
(** [draw sw] is [sw] with the top card in stock removed and placed as the new
    top card in waste. If the stock is empty, it will put the cards from waste
    back into stock*)

val top_sw : t -> Card.t
(**[top] is the top card in the waste pile. Raises EmptyWaste if the waste pile
   is empty.*)

(* Desmond used this to test his code . May be needed sometime. do not delete
   except for Desmond *)
(* val getStock : t -> Card.t list *)

val remove_top : t -> t
(** [remove_top sw] is [sw] with [top sw] removed and with a new top card.
    Raises EmptyWaste if the waste pile is empty*)

val check_stock_empty : t -> bool
(**[check_stock_empty sw] is [true] if the stock is empty and [false] if it is
   not*)

val get_waste : t -> Card.t list
(**[get_waste sw] is the waste pile represented as a Card.t list*)
