type t = {
  name : string;
  bg : ANSITerminal.style;
  red : ANSITerminal.style;
  black : ANSITerminal.style;
  faces : ANSITerminal.style;
  backs : ANSITerminal.style;
  back_pattern : ANSITerminal.style;
  text : ANSITerminal.style;
}
(** [type t] is the exposed dictionary type of theme, so external functions may
    retrieve data from it with just the [.] notation. [red], [black],
    [back_pattern], [text] are foreground styles (in ANSITERMINAL) and [bg],
    [faces], [backs] are background styles. *)

exception UnknownTheme
(** [UnknownTheme] indicates that a theme attempted to be referenced does not
    exist. *)

val theme_of_string : string -> t
(** [theme_of_string str] is the theme a string corresponds to. Raises
    [UnknownTheme] if the string does not match to a variant.*)
