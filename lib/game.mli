open Card

type t

val new_game : t
(**[new_game] is game of solitaire in the starting position, with cards
   randomized.*)

val draw_card : t -> t
(**[draw_card fsb] is a game with the draw function applied to the stockwaste.*)

val formatted : t -> (bool * Card.t list) * Card.t list * Card.t list list
(** [formatted g] is a triple [(s, f, b)] representing the states of the
    stockwaste, foundation, and tableau. [s] is if the stock is empty and a list
    of cards in the waste. [f] is the top cards of the foundation (length 4).
    [b] has length 7 (one for each column), with hidden cards represented by
    [empty_card]. *)
