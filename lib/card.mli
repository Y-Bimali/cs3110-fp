type suit =
  | Spades
  | Hearts
  | Clubs
  | Diamonds  (**[suit] represents the suit of a card deck*)

type color =
  | Red
  | Black  (**[color] represents the color of a card*)

type rank =
  | Ace
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Zero  (**[rank] represents the rank of the card*)

type t = {
  rank : rank;
  suit : suit;
}
(** The type representing a card, consisting of a rank and a suit. *)

val string_of_suit : suit -> string
(** [string_of_suit s] returns a string representation of the given suit [s]. *)

val rank_of : t -> rank
(** [rank_of card] returns the rank of the given card. *)

val suit_of : t -> suit
(** [suit_of card] returns the suit of the given card. *)

val num_of : t -> int
(** [num_of card] returns the numerical value of the rank of the given card. *)

val string_of_rank : t -> string
(** [string_of_rank card] returns a string representation of the rank of the
    given card. *)

val color_of : t -> color
(** [color_of card] returns the color of the given card. *)

val to_string : t -> string
(** [to_string card] returns a string representation of the given card. *)

val empty_card : suit -> t
(** [empty_card suit] is an [empty_card s] is an "empty" card of suit [s] and
    rank [Zero] *)

exception UnusableRank
(** [UnusableRank] is called if a supplied rank is not between 0 and 13.*)

val new_card : suit -> int -> t
(** [new_card s n] is a card of suit [s] and the rank corresponding to [n].
    Raises [UnusableRank] if n is not a value between 0 and 13.*)
