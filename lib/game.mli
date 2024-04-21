type t

val new_game : unit -> t
(**[new_game] is game of solitaire in the starting position, with cards
   randomized.*)

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

val s_to_f : t -> t * string option
(** [s_to_f g] is ???*)
val move_card_to_foundation: t -> int -> t * string option

val move_matching_card_to_tableau : t -> int -> int-> t * string option