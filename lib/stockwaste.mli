open Card

type t

val draw : t -> t
(** [draw] sw is sw with the top card in stock removed and placed as the new top card in waste*)

val top : t -> Card.t
(**[top] is the top card in the waste pile. Requires that waste pile is not empty *)
(** MAKE THIS AN OPTION or check (isempty)*)

val remove_top : t -> t
(** [remove_top sw] is [sw] with [top sw] removed and with a new top card.*)
