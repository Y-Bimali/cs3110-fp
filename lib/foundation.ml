open Card

(** AF: The record [{spade = s, heart = h, club = c, diamond = d}] represents
    the four foundations with s, h, c, and d on top. RI: The the suit of the
    Card stored in the field matches the name of the field*)

type t = {
  spade : Card.t;
  heart : Card.t;
  club : Card.t;
  diamond : Card.t;
}

exception InvalidMove

let rep_ok f =
  if
    suit_of f.spade = Spades
    && suit_of f.heart = Hearts
    && suit_of f.club = Clubs
    && suit_of f.diamond = Diamonds
  then f
  else failwith "RI"

let initialize =
  rep_ok
    {
      spade = empty_card Spades;
      heart = empty_card Hearts;
      club = empty_card Clubs;
      diamond = empty_card Diamonds;
    }

let top_cards f =
  let f = rep_ok f in
  [ f.spade; f.heart; f.club; f.diamond ]

let is_complete f =
  List.fold_left (fun acc x -> acc && rank_of x = King) true (top_cards f)

let set s h c d = rep_ok { spade = s; heart = h; club = c; diamond = d }

let won_foundation =
  set (new_card Spades 13) (new_card Hearts 13) (new_card Clubs 13)
    (new_card Diamonds 13)

let suit_matcher fcn f c =
  match suit_of c with
  | Spades -> fcn f.spade c
  | Hearts -> fcn f.heart c
  | Clubs -> fcn f.club c
  | Diamonds -> fcn f.diamond c

let card_swap f c =
  match suit_of c with
  | Spades -> { f with spade = c }
  | Hearts -> { f with heart = c }
  | Clubs -> { f with club = c }
  | Diamonds -> { f with diamond = c }

let valid_move f c =
  let f = rep_ok f in
  suit_matcher (fun fc ac -> num_of ac = num_of fc + 1) f c

let valid_remove f c =
  let f = rep_ok f in
  suit_matcher (fun fc ac -> num_of ac = num_of fc && num_of ac > 0) f c

let decrement c = new_card (suit_of c) (num_of c - 1)

let put f c =
  let f = rep_ok f in
  if valid_move f c then card_swap f c else raise InvalidMove

let remove f c =
  let f = rep_ok f in
  if valid_remove f c then card_swap f (decrement c) else raise InvalidMove
