exception InvalidMove
(** [InvalidMove] is raised when an attempt is made to [put] a card on the
    foundation that is not allowed by the game rules, or [remove] a specified
    card from the foundation that is not on the top of a stack.*)

type t
(**[t] is the type of the data structure representing a foundation.*)

val valid_move : t -> Card.t -> bool
(**[valid_move f c] checks if [c] fits correctly on [f]. "Correctly" is in terms
   of game mechanics, where [c] must have a rank of one greater than the top
   card of its suit, or be an [Ace] if there are no cards of its suit in the
   foundation.*)

val put : t -> Card.t -> t
(**[put f c] adds [c] to [f]. Raises [InvalidMove] if [valid_move f c] is false.*)

val top_cards : t -> Card.t list
(**[top_cards f] is a list of the top cards in [f]. This is always in the order
   [spade; heart; club; diamond]. If no cards exist in the foundation of a suit,
   an empty card of that suit takes that position in the list.*)

val initialize : t
(**[initialize] is an empty foundation.*)

val is_complete : t -> bool
(**[is_complete f] is if the foundation is all Kings (the game is won).*)

val remove : t -> Card.t -> t
(**[remove f c] is [f] without [c]. Raises [InvalidMove] if [c] is not in
   [top_cards] or [c] is an empty card. *)

val set : Card.t -> Card.t -> Card.t -> Card.t -> t
(**[set s h c d] is the foundation with the top cards [s] for spades, [h] for
   hearts, [c] for clubs, and [d] for diamonds. Requires: suits of [s], [h],
   [c], and [d] match the given order.*)

val won_foundation : t
(**[won_foundation] is the foundation with all Kings as top cards.*)
