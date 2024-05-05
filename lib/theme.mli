type t = {
  name : string;
  bg : ANSITerminal.style;
  red : ANSITerminal.style;
  black : ANSITerminal.style;
  faces : ANSITerminal.style;
  backs : ANSITerminal.style;
  back_pattern : ANSITerminal.style;
}
(** Exposed dictionary type of theme, so external functions may retrieve data
    from it with just the [.] notation. [red], [black], [back_pattern] are
    foreground styles (in ANSITERMINAL) and [bg], [faces], [backs] are
    background styles. *)

exception UnknownTheme

val theme_of_string : string -> t
(** [theme_of_string str] is the theme a string corresponds to. Raises
    [UnknownTheme] if the string does not match to a variant.*)
