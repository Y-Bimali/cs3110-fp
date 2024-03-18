(* open Card

   type t (** [t] represents the ... *)

   type column (** [column] represents the space to keep the piles. [column] is
   either 0,1,2,3,4,5,or 6*)

   val card_from_col : t -> column -> Card.t (** [card_from_col tab col] is the
   top card of [col].*)

   val take_to_foundation : t -> column -> t (** [take_to_foundation tab col]
   moves bottom card in [col] to foundation. Must be called with corresponding
   [Foundation.put] call.*)

   val take_to_other_col : t -> column -> column -> int -> t (**
   [take_to_other_col tab c1 c2 i] is tableau [tab] after moving the bottom [i]
   cards from column [c1] to [c2]. If not possible, nothing happens. *)

   val valid_move : t -> column -> Card.t -> bool (** [valid_move tab col card]
   checks if topmost visible [card] can be moved [col] whose visible card is one
   higher *)

   val card_to_col : t -> column -> Card.t -> t (** [card_to_col tab col card]
   is [tab] after putting [card] onto [col]. Requires: [valid_move col card =
   true]. *)

   val to_string : t -> string (**[to_string t] is the tableau represented as a
   string(SPECIFY WHEN IMPLEMENTED)*) *)
