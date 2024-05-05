type t

val new_game : unit -> t
(**[new_game] is game of solitaire in the starting position, with cards
   randomized.*)

val game_from_parts : Foundation.t -> Stockwaste.t -> Tableau.t -> t
(** [game_from_parts] is a game of solitaire initialized from a foundation,
    stockwaste, and tableau.*)

val draw_card : t -> t * string option
(**[draw_card fsb] is a game with the draw function applied to the stockwaste.*)

val formatted : t -> (bool * Card.t list) * Card.t list * Card.t list list
(** [formatted g] is a triple [(s, f, b)] representing the states of the
    stockwaste, foundation, and tableau. [s] is if the stock is empty and a list
    of cards in the waste. [f] is the top cards of the foundation (length 4).
    [b] has length 7 (one for each column), with hidden cards represented by
    [empty_card]. *)

(* desmond created these to check his code. May be needed sometime. do not
   delete except for Desmond *)
(* val generate_deck : Card.t list *)
(* val get_foundation : t -> Foundation.t val get_stockwaste : t -> Stockwaste.t
   val get_tableau : t -> Tableau.t *)

val undo : t -> t * string option
(** [undo g] is (g', opt) where, if the previous game state exists, g' is the
    previous game state and opt is None. If this is the earlierst game state, g'
    is [g] and opt contains an error message.*)

val s_to_f : t -> t * string option
(** [s_to_f g] is (g, opt) where g is the game that could have been updated with
    the top card of the stock moving to the foundation or the original game. opt
    is the option where None means that the card successfully moved and Some h
    means that the card did not successfully move and h is the reason why
    represented as a string.*)

val s_to_t : t -> int -> t * string option
(** [s_to_t g tab_index] is (g, opt) where g is the game that could have been
    updated with the top card of the stock moving to the tableau at index
    tab_index or the original game. opt is the option where None means that the
    card successfully moved and Some h means that the card did not successfully
    move and h is the reason why represented as a string..*)

val move_tableau_card_to_foundation : t -> int -> t * string option
(** [move_tableau_card_to_foundation g idx] is (g, opt) where g is the game that
    could have been updated by moving the bottom card from column [idx] to the
    right column in the foundation. opt is the option where None means the
    card(s) successfully moved and Some h means that the card did not
    successfully move and h is the reason why, represented as a string.*)

val move_card_from_foundation_to_tableau : t -> int -> int -> t * string option
(** [move_card_from_foundation_to_tableau g i j] is (g, opt) where g is the game
    that could have been updated with the top card of the [i]th column in
    foundation moving to the [j]th column in the tableau or the original game.
    opt is the option where None means that the card successfully moved and Some
    h means that the card did not successfully move and h is the reason why
    represented as a string.*)

val t_to_t : t -> string -> string -> t * string option
(** [t_to_t g c1 c2] is (g', opt) where g' is the game that could have been
    updated by moving cards from column [c1] to column [c2] in the tableau. opt
    is the option where None means the card(s) successfully moved and Some h
    means that the card did not successfully move and h is the reason why,
    represented as a string.*)

val check_win : t -> bool
(** [check_win t] is true if t is in a winning state and false otherwise.*)

val autowin : t -> t * string option
(** [autowin g] is (g', opt) where g' is the winning state if [g] is an
    autowinnable state, and g otherwise. opt is the option Some h describing the
    outcome of [autowin g].*)

val cheat : t -> string -> string -> t * string option
(** [cheat g col card] is (g, opt) where g is the same game and opt is the
    string option Some h with h describing the [card]th card at column [col]. If
    the specified card does not exist, Some h instead contains a message
    explaining so.*)

val won_game : t
(** [won_game] is the won game. *)

val update_three_opt : string option -> unit
(** [update_three_opt o] sets whether the game is draw 3 or draw 1. Requires:
    [o] is [None] or [Some "3"].*)

val get_count : unit -> int
(** [get_count] is the number of moves made in this game.*)

val get_undos : unit -> int
(** [get_undos] is the number of undos made in this game.*)
