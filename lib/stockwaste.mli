type t
(**[t] is the type of the data structure representing a stockwaste.*)

val empty_sw : t
(**[empty_sw] is the empty stock and the empty waste*)

val add_sw : Card.t list -> t -> t
(**[add_sw] adds a card list to the stock*)

val size_sw : t -> int * int
(**[size_sw] is a pair where the first term is the size of the stock and the
   second term is the size of the waste.*)

val draw : (unit -> string option) -> t -> t option
(** [draw sw three_opt] is [Some sw] with the top card(s) in stock removed and
    placed as the new top card(s) in waste. If the stock is empty, it will put
    the cards from waste back into stock. If the stock and the waste are empty,
    it be [None]*)

val top_sw : t -> Card.t option
(**[top] is Some top card in the waste pile. Is [None] if the waste pile is
   empty.*)

val remove_top : t -> t option
(** [remove_top sw] is [Some sw] with [top sw] removed and with a new top card.
    None if the waste pile is empty*)

val check_stock_empty : t -> bool
(**[check_stock_empty sw] is [true] if the stock is empty and [false] if it is
   not*)

val get_waste : t -> Card.t list
(**[get_waste sw] is the waste pile represented as a Card.t list*)
