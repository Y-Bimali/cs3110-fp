open Card

type t

type column =  0 | 1 | 2 | 3 | 4 | 5 | 6 

(** [card_from_col tab col] is the top card of [col].*)
val card_from_col : t -> column -> Card.t


(** [take_to_foundation tab col] moves bottom card in [col] to foundation. Must be 
    called with corresponding [Foundation.put] call.*)
val take_to_foundation : t -> column -> t

(** [take_to_other_col tab c1 c2 i] is tableau [tab] after moving the bottom [i]
     cards from column [c1] to [c2]. If not possible, nothing happens. *)
val take_to_other_col : t -> column -> column -> int -> t
 
(** [valid_move tab col card]  checks if topmost visible [card] can be moved [col] whose
    visible card is one higher *)
val valid_move : t -> column -> Card.t -> bool

(** [card_to_col tab col card] is [tab] after putting [card] onto [col]. 
    Requires: [valid_move col card = true]. *)
val card_to_col : t -> column -> Card.t -> t

(**[to_string t] is the tableau represented as a string(SPECIFY 
    WHEN IMPLEMENTED)*)
val to_string : t -> string



