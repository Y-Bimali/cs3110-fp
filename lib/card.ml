type suit =
  | Spades
  | Hearts
  | Clubs
  | Diamonds

type color =
  | Red
  | Black

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

type t = {
  rank : rank;
  suit : suit;
}

let string_of_suit = function
  | Hearts -> "♥"
  | Diamonds -> "♦"
  | Clubs -> "♣"
  | Spades -> "♠"

let rank_of card = card.rank
let suit_of card = card.suit

let num_of card =
  match card.rank with
  | Ace -> 1
  | Two -> 2
  | Three -> 3
  | Four -> 4
  | Five -> 5
  | Six -> 6
  | Seven -> 7
  | Eight -> 8
  | Nine -> 9
  | Ten -> 10
  | Jack -> 11
  | Queen -> 12
  | King -> 13
  | Zero -> 0

let string_of_rank card =
  match card.rank with
  | Ace -> "A"
  | Jack -> "J"
  | Queen -> "Q"
  | King -> "K"
  | _ -> string_of_int (num_of card)

let color_of card =
  match card.suit with
  | Hearts | Diamonds -> Red
  | Spades | Clubs -> Black

let to_string card =
  let suit = string_of_suit card.suit in

  let rank = string_of_rank card in

  rank ^ suit

let empty_card suit = { rank = Zero; suit }
