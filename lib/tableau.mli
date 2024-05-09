type t
(** [t] represents the tableau. AF: *)

val init_tab : Card.t list -> t
(** [init_tab lst] is a tableau initialized from a list of cards. Requires:
    [lst] is a list of 28 cards.*)

val empty_tab : t
(** [empty_tab] is the empty tableau.*)

exception InvalidColID
(** Represents an attempt to access a nonexistent column.*)

exception InvalidCardID
(** Represents an attempt to access a nonexistent card.*)

exception IllegalMove
(** Represents an attempt to move cards illegally.*)

exception EmptyCol
(** Represents an attempt to pop from an empty column.*)

val peek_col_card : t -> int -> Card.t option
(** [peek_col_card tab col] is the top card of column [col], if it exists.
    Raises: [InvalidColID] if [0 > [col] || [col] > 6].*)

val pop_col_card : t -> int -> t * Card.t
(** [pop_col_card tab col] is the pair of [tab] after removing the bottommost
    card from [col] and the card itself. Must be called with corresponding
    [Foundation.put] call. Raises: [InvalidColID] if [0 > [col] || [col] > 6].
    [EmptyCol] if [col] has no cards. *)

val cheat_col_card : t -> int -> int -> Card.t
(** [cheat_col_card col cardi] is the [cardi] card from the top of column [col].
    Raises: [InvalidColID] if [0 > [col] || [col] > 6]. [InvalidCardID] if [col]
    has fewer cards than [cardi].*)

val card_to_col : t -> int -> Card.t -> t
(** [card_to_col tab col card] is [tab] after putting [card] onto [col]. Raises:
    [InvalidColID] if [0 > [col] || [col] > 6]. [IllegalMove] if this move is
    not legal. *)

val move_col_to_col : t -> int -> int -> t
(**[move_col_to_col tab
      c1 c2] is tableau [tab] after moving cards from
   column [c1] to [c2]. Raises: [InvalidColID] if
   [0 > [c1] || [c1] > 6 || 0 > [c1] || [c1]
      > 6]. [IllegalMove] if this
   move is not legal.*)

val lowest_col_index : t -> int option
(** [lowest_col_index t] is the index of the column of [t] containing a
    lowest-numbered card, or [None] if [t = empty_tab].*)

val winnable : t -> bool
(** [winnable tab] is [true] if [tab] is in a winnable state, and [false]
    otherwise.*)

val to_str_lst : t -> string list list
(**[to_string t] is the tableau represented as a string list.*)

val to_cd_lst : t -> Card.t list list
(** [to_cd_lst t] is the tableau represented as a card list list.*)

(* val to_str : t -> string *[to_string t] is the tableau represented as a
   string. *)
