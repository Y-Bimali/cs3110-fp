open Card

type t
(**[t] is the type of the data structure implementing foundation*)

val valid_move : t -> Card.t -> bool
(**[valid_move f c] checks if [c] fits correctly on [f]*)

val put : t -> Card.t -> t
(**[put f c] adds [c] to [f]. Requires [valid_move f c] is true (unless we implement this as an option)*)

val top_cards : t -> Card.t list
(**[top_cards f] is a list of the top cards in [f]*)

val initialize : t
(**[initialize] is an empty foundation*)

val is_complete : t -> bool
(**[is_complete f] is if the foundation is all Kings (the game is won)*)

val remove : t -> Card.t -> t
(**[remove f c] is [f] without [c]. Requires that [c] is in [top_cards], and [c] is not an empty card.*)
