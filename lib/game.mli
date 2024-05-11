type t
(** [t] represents the game including the current physical position (stockwaste,
    tableau, foundation) and associated data (moves, undos, previous game
    versions, and start time).*)

val new_game : unit -> t
(** [new_game] is game of draw-1 solitaire in the starting position, with cards
    randomized. It has 0 moves, 0 undos, no previous versions, and a start time
    of about when the function is called.*)

val game_from_parts : Foundation.t -> Stockwaste.t -> Tableau.t -> t
(** [game_from_parts f s b] is a game of solitaire initialized from a foundation
    [f], stockwaste [s], and tableau [b]. It has 0 moves, 0 undos, no previous
    versions, and a start time of about when the function is called.*)

val formatted : t -> (bool * Card.t list) * Card.t list * Card.t list list
(** [formatted g] is a triple [(s, f, b)] representing the states of the
    stockwaste, foundation, and tableau. [s] is if the stock is empty and a list
    of cards in the waste. [f] is the top cards of the foundation (length 4).
    [b] has length 7 (one for each column), with hidden cards represented by
    [empty_card]. *)

val draw_card : t -> t * string option
(**[draw_card g] is [g] with the draw function applied to the stockwaste.*)

val undo : t -> t * string option
(** [undo g] is [(g', opt)] where, if the previous game state exists, [g'] is
    the previous game state and opt is None. If this is the earliest game state,
    [g'] is [g] and opt contains an error message. If [g'] <> [g], get_count[g']
    = get_count[g] - 1, and [get_undos g = get_undos g' + 1].*)

val s_to_f : t -> t * string option
(** [s_to_f g] is (g', opt) where g' is [g] with the top card of the stock
    moving to the foundation or [g] if that is not possible/permitted. opt is
    the option where None means that the card successfully moved and Some h
    means that the card did not successfully move and h is the reason why
    represented as a string. If [g'] <> [g], get_count[g'] = get_count[g] + 1.*)

val s_to_t : t -> int -> t * string option
(** [s_to_t g tab_index] is (g', opt) where g' is [g] with the top card of the
    stock moving to the tableau at index tab_index or [g] if that is not
    possible/permitted. opt is the option where None means that the card
    successfully moved and Some h means that the card did not successfully move
    and h is the reason why represented as a string. If [g'] <> [g],
    get_count[g'] = get_count[g] + 1.*)

val move_tableau_card_to_foundation : t -> int -> t * string option
(** [move_tableau_card_to_foundation g idx] is (g', opt) where g' is [g] with
    the bottom card from column [idx] moved to the correct position in the
    foundation, or [g] if that is not possible/permitted. opt is the option
    where None means the card successfully moved and Some h means that the card
    did not successfully move and h is the reason why, represented as a string.
    If [g'] <> [g], get_count[g'] = get_count[g] + 1.*)

val move_card_from_foundation_to_tableau : t -> int -> int -> t * string option
(** [move_card_from_foundation_to_tableau g i j] is (g', opt) where g' is [g]
    with the top card of the [i]th column in foundation moving to the [j]th
    column in the tableau or [g] if that is not possible/permitted. opt is the
    option where None means that the card successfully moved and Some h means
    that the card did not successfully move and h is the reason why represented
    as a string. If [g'] <> [g], get_count[g'] = get_count[g] + 1.*)

val t_to_t : t -> string -> string -> t * string option
(** [t_to_t g c1 c2] is (g', opt) where g' is [g] with a nonzero amount of cards
    (legally) moved from column [c1] to column [c2] in the tableau, or [g] if
    that is not possible/permitted. opt is the option where None means the
    card(s) successfully moved and Some h means that the card did not
    successfully move and h is the reason why, represented as a string. If [g']
    <> [g], get_count[g'] = get_count[g] + 1.*)

val check_win : t -> bool
(** [check_win t] is true if [t] is in a winning state and false otherwise.*)

val autowin : t -> t * string option
(** [autowin g] is (g', opt) where g' is the winning state if [g] is an
    autowinnable state, and [g] otherwise. opt is the option Some h describing
    the outcome of [autowin g]. If [g'] <> [g], get_count[g'] = get_count[g] +
    the number of cards in [g]'s tableau.*)

val autowin_gamelist : t -> t list
(** [autowin_gamelist g] is the list of game states that [g] must progress
    through to be won. Requires: [autowin g = true].*)

val cheat : t -> string -> string -> t * string option
(** [cheat g col card] is (g, opt) where [g] is the same game and opt is the
    string option Some h with h describing the [card]th card at column [col]. If
    the specified card does not exist, Some h instead contains a message
    explaining so.*)

val won_game : t
(** [won_game] is the won game. *)

val update_three_opt : string option -> t -> t
(** [update_three_opt o g] is game [g] with the draw style set to draw 3 or draw
    1 (depending on [o]). Requires: [o] is [None] or [Some "3"].*)

val get_count : t -> int
(** [get_count g] is the number of moves made in game [g].*)

val get_undos : t -> int
(** [get_undos g] is the number of undos made in game [g].*)

val start_time : t -> float
(** [start_time g] is the start time (in Unix time) of game [g] *)
