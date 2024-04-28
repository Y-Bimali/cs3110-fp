(** The signature for a theme. Requires: red, black, back_pattern are foreground
    styles (in ANSITERMINAL) and bg, faces, and backs are background styles. *)
module type T = sig
  val name : string
  val bg : ANSITerminal.style
  val red : ANSITerminal.style
  val black : ANSITerminal.style
  val faces : ANSITerminal.style
  val backs : ANSITerminal.style
  val back_pattern : ANSITerminal.style
end

(** The modules for available themes. *)

module MClassic : T
module MSpaceship : T
module MUnderTheSea : T

(** These variants each correspond to a themes module and can easily be passed
    around as values, but will need to be decoded/pattern matched to a module
    externally. *)
type t =
  | Classic
  | Spaceship
  | UnderTheSea

exception UnknownTheme

val theme_of_string : string -> t
(** [theme_of_string str] is the theme variant a string corresponds to. Raises
    [UnknownTheme] if the string does not match to a variant.*)
