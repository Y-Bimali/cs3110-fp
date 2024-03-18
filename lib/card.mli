

type suit =
  | Spades
  | Hearts
  | Clubs
  | Diamonds  (**[suit] represents the suit of a card*)

type color = Red | Black  (**[color] represents the color of a card*)

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
  | Zero  
  (**[rank] represents the rank of the card*)

type t
(**[t] is the data structure of a card*)

val to_string : t -> string
(**[to_string c] is the card represented as a string(SPECIFY WHEN IMPLEMENTED)*)

val rank_of : t -> rank
(**[rank_of c] is the rank of [c]  *)
val suit_of : t -> suit
(**[suit_of c] is the suit of [c]*)
val num_of : t -> int
(**[num_of c] is the integer corresponding to the rank of [c]*)
val color_of : t -> color
(**[color_of c] is the color of [c]*)

val empty_card : suit -> t
(**[empty_card s] is a card of suit [s] and rank [Zero]*)
