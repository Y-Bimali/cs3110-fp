open ANSITerminal

module type T = sig
  val name : string
  val bg : ANSITerminal.style
  val red : ANSITerminal.style
  val black : ANSITerminal.style
  val faces : ANSITerminal.style
  val backs : ANSITerminal.style
  val back_pattern : ANSITerminal.style
end

let color_parser foreground name =
  let name = String.lowercase_ascii name in
  List.nth
    (match name with
    | "black" -> [ on_black; black ]
    | "red" -> [ on_red; red ]
    | "green" -> [ on_green; green ]
    | "yellow" -> [ on_yellow; yellow ]
    | "blue" -> [ on_blue; blue ]
    | "magenta" -> [ on_magenta; magenta ]
    | "cyan" -> [ on_cyan; cyan ]
    | "white" -> [ on_white; white ]
    | _ -> failwith "Improperly formatted or spelled input color theme")
    (Bool.to_int foreground)

let data_in =
  List.map (String.split_on_char ',')
    (BatList.of_enum (BatFile.lines_of "data/themes.txt"))

module Make (S : sig
  val data : string list
end) =
struct
  let name = List.nth S.data 0
  let bg = color_parser false (List.nth S.data 1)
  let red = color_parser true (List.nth S.data 2)
  let black = color_parser true (List.nth S.data 3)
  let faces = color_parser false (List.nth S.data 4)
  let backs = color_parser true (List.nth S.data 5)
  let back_pattern = color_parser false (List.nth S.data 6)
end

(** Add the module for new themes below, with the int argument for List.nth
    being the corresponding line of the themes.txt file. By convention, place a
    capital M in fron to distinguish it from the variant name. *)

module MClassic : T = Make (struct
  let data = List.nth data_in 0
end)

module MSpaceship : T = Make (struct
  let data = List.nth data_in 1
end)

module MUnderTheSea : T = Make (struct
  let data = List.nth data_in 2
end)

(** Add the corresponding variant name here, and fix all code pattern matching
    to this type.*)

type t =
  | Classic
  | Spaceship
  | UnderTheSea

exception UnknownTheme

(** Add a string name for the theme here to help functions decoding strings to
    themes. You may add multiple names for the same theme if you find it
    sensible. *)
let theme_of_string str =
  match String.lowercase_ascii str with
  | "classic" -> Classic
  | "spaceship" -> Spaceship
  | "under the sea" -> UnderTheSea
  | _ -> raise UnknownTheme
