open ANSITerminal

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

exception UnknownTheme

let theme_list = ref []

let rec find_theme str lst =
  match lst with
  | [] -> raise UnknownTheme
  | h :: t ->
      if str = String.lowercase_ascii h.name then h else find_theme str t

let theme_of_string str = find_theme str !theme_list

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

let make data =
  theme_list :=
    {
      name = List.nth data 0;
      bg = color_parser false (List.nth data 1);
      red = color_parser true (List.nth data 2);
      black = color_parser true (List.nth data 3);
      faces = color_parser false (List.nth data 4);
      backs = color_parser true (List.nth data 5);
      back_pattern = color_parser false (List.nth data 6);
      text = color_parser true (List.nth data 7);
    }
    :: !theme_list

let data_in =
  List.map (String.split_on_char ',')
    (BatList.of_enum (BatFile.lines_of "data/themes.txt"))

let () =
  for i = 0 to List.length data_in - 1 do
    make (List.nth data_in i)
  done
