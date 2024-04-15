type theme = {
  bg : ANSITerminal.style;
  red : ANSITerminal.style;
  black : ANSITerminal.style;
  backs : ANSITerminal.style;
  back_pattern : ANSITerminal.style;
  faces : ANSITerminal.style;
}
(** [theme] contains information on coloring the background, red cards, black
    cards, backs of cards, patterns on backs of cards, and faces of cards.*)

let dt =
  {
    bg = ANSITerminal.on_green;
    red = ANSITerminal.red;
    black = ANSITerminal.black;
    backs = ANSITerminal.on_white;
    back_pattern = ANSITerminal.blue;
    faces = ANSITerminal.on_white;
  }
(* A default theme *)

type card_style =
  | Top
  | Full
  | Left
  | NoneFull
  | NoneLeft
  | NoneTop
  | Back
  | BackTop

(** [reformat_card c] inserts a whitespace to the left of [c]'s string
    representation if it is only two characters long, or returns [c]'s string
    representation otherwise.*)
let reformat_card c =
  let s = Card.to_string c in
  if Card.num_of c <> 10 then " " ^ s else s

let print_board b =
  List.fold_left
    (fun _ lst ->
      print_endline "";
      List.fold_left
        (fun _ x ->
          match x with
          | params, str -> ANSITerminal.print_string params str)
        () lst)
    () b

let blank x = String.make x (String.get " " 0)
let back_blank theme x = ([ theme.bg ], blank x)
let blank_row t = back_blank t 49
let norm_card_strings c = [ reformat_card c ^ "  "; " XXX "; " XXX "; "     " ]
let left_card_strings c = [ reformat_card c ^ "|"; " XX|"; " XX|"; "   |" ]

let color t c =
  match Card.color_of c with
  | Card.Black -> t.black
  | Card.Red -> t.red

let card_tagged theme style c =
  let form =
    match style with
    | Top | Full | Left -> [ color theme c; theme.faces ]
    | NoneFull | NoneLeft | NoneTop -> [ theme.bg ]
    | Back | BackTop -> [ theme.back_pattern; theme.backs ]
  in
  match style with
  | Top -> [ (form, reformat_card c ^ "  ") ]
  | Full -> List.map (fun x -> (form, x)) (norm_card_strings c)
  | Left -> List.map (fun x -> (form, x)) (left_card_strings c)
  | NoneFull -> List.map (fun x -> (form, x)) (List.init 4 (fun _ -> blank 5))
  | NoneLeft -> List.map (fun x -> (form, x)) (List.init 4 (fun _ -> blank 4))
  | NoneTop -> [ (form, blank 5) ]
  | Back -> List.map (fun x -> (form, x)) [ "XXXXX"; "X###X"; "X###X"; "XXXXX" ]
  | BackTop -> [ (form, "XXXXX") ]

let stock_app theme s =
  if s then card_tagged theme NoneFull (Card.empty_card Card.Spades)
  else card_tagged theme Back (Card.empty_card Card.Spades)

let rec t_helper out_lst in_lst =
  match List.nth in_lst 0 with
  | [] -> List.rev out_lst
  | _ :: _ ->
      let a, b =
        List.fold_left_map
          (fun acc xs ->
            match xs with
            | [] -> failwith "Nonrectangular Table"
            | h2 :: t2 -> (h2 :: acc, t2))
          [] in_lst
      in
      t_helper (List.rev a :: out_lst) b

let transpose lst = t_helper [] lst

let waste_app theme w =
  let emp = Card.empty_card Card.Spades in
  match List.length w with
  | 0 ->
      transpose
        [
          card_tagged theme NoneLeft emp;
          card_tagged theme NoneLeft emp;
          card_tagged theme NoneFull emp;
        ]
  | 1 ->
      transpose
        [
          card_tagged theme NoneLeft emp;
          card_tagged theme NoneLeft emp;
          card_tagged theme Full (List.nth w 0);
        ]
  | 2 ->
      transpose
        [
          card_tagged theme NoneLeft emp;
          card_tagged theme Left (List.nth w 1);
          card_tagged theme Full (List.nth w 0);
        ]
  | _ ->
      transpose
        [
          card_tagged theme Left (List.nth w 2);
          card_tagged theme Left (List.nth w 1);
          card_tagged theme Full (List.nth w 0);
        ]

let found_app_mod theme c =
  if Card.num_of c = 0 then card_tagged theme NoneFull c
  else card_tagged theme Full c

let found_app theme f = List.map (found_app_mod theme) f

let tab_app_mod theme booled_c =
  match booled_c with
  | b, c ->
      if b = false then card_tagged theme NoneTop c
      else if Card.num_of c = 0 then card_tagged theme BackTop c
      else card_tagged theme Top c

let tab_app theme booled_t =
  List.map
    (fun x ->
      List.map
        (fun y ->
          match y with
          | a :: [] -> a
          | _ -> failwith "Not Card Top")
        x)
    (transpose (List.map (List.map (tab_app_mod theme)) booled_t))

let rec top_help lst s w_lst f_lst theme =
  match (s, w_lst, f_lst) with
  | [], [], [] -> lst
  | hs :: ts, hw :: tw, hf :: tf ->
      let bb = back_blank theme 1 in
      top_help
        (lst
        @ [
            [
              bb;
              hs;
              bb;
              List.nth hw 0;
              List.nth hw 1;
              List.nth hw 2;
              back_blank theme 5;
              List.nth hf 0;
              bb;
              List.nth hf 1;
              bb;
              List.nth hf 2;
              bb;
              List.nth hf 3;
              bb;
            ];
          ])
        ts tw tf theme
  | _ -> failwith "Incompatible Formats"

let format_top theme s w f =
  [ [ blank_row theme ] ]
  @ top_help [] (stock_app theme s) (waste_app theme w) (found_app theme f)
      theme
  @ [ [ blank_row theme ] ]

let print_top theme g =
  match Game.formatted g with
  | (s, w), f, _ -> print_board (format_top theme s w f)

let rec pad_out rem padding out_lst in_lst =
  if rem = 0 then List.rev out_lst
  else
    match in_lst with
    | [] -> pad_out (rem - 1) padding (padding :: out_lst) []
    | h :: t -> pad_out (rem - 1) padding (h :: out_lst) t

let justify len lsts padding = List.map (pad_out len padding []) lsts

let rec alternate sep out_lst in_lst =
  match in_lst with
  | [] -> []
  | a :: [] -> List.rev (a :: out_lst)
  | h :: t -> alternate sep (sep :: h :: out_lst) t

let format_tab theme t =
  let flipped_t = List.map List.rev t in
  let booled_t = List.map (List.map (fun x -> (true, x))) flipped_t in
  let len = List.fold_left (fun acc lst -> max acc (List.length lst)) 0 t in
  let justified = justify len booled_t (false, Card.empty_card Card.Spades) in
  List.map
    (fun x ->
      (back_blank theme 1 :: alternate (back_blank theme 2) [] x)
      @ (back_blank theme 1 :: []))
    (tab_app theme justified)

let print_tab theme g =
  match Game.formatted g with
  | _, _, t -> print_board (format_tab theme t)

(*let board g = match Game.formatted g with ((s, w), f, t) -> let len = 10 +
  List.fold_left (fun acc x -> max acc (List.length x)) 0 t in;;*)

(**[query pg q v e r] is a valid response to a prompted user interaction in the
   terminal. The function prints [q] prompts a response from the command line,
   validates the string response [the_input] with [v the_input], and prints [r]
   as a follow up to a valid response (no newline printed if [r] is the empty
   string). If the response is invalid, it describes the error using [e] and
   reprompts.*)
let rec query pg q v e r =
  match pg with
  | b, g ->
      if b then
        let () = print_top dt g in
        print_tab dt g
      else ();
      print_endline "";
      print_string q;
      let the_input = String.uppercase_ascii (read_line ()) in
      if v the_input then (
        if r = "" then () else print_endline r;
        the_input)
      else
        let () = print_endline e in
        query pg q v e r

let commands = [ "NEW GAME"; "DRAW"; "D"; "QUIT"; "S TO F"; "T TO F"; "F TO T" ]

(** Hint: split_on_char : char -> string -> string list, which char being a
    space ' '.*)
let validate s =
  match s with
  | "PATTERN MATCH YOUR MOVE HERE" -> failwith "Unimplemented"
  | _ -> List.mem s commands

let print_error e =
  match e with
  | None -> ()
  | Some s -> print_endline s

(**TODO: output tuple should be bool * game Functions called in Game should
   return game * string option *)
let round g =
  let q =
    query (true, g) "Enter an action: " validate "Unrecognizable Command."
      "Successful Command."
  in
  if q = "QUIT" then (false, g)
  else
    let g2, error =
      match q with
      | "DRAW" | "D" -> Game.draw_card g
      | "NEW GAME" -> (Game.new_game (), None)
      | "S TO F" -> Game.s_to_f g
      | "T TO F" ->
        
          let () = print_string "Enter the tableau column index: " in
          let col_index = read_int () in
          Game.move_card_to_foundation g col_index
      | "F TO T" ->
          
          let () = print_string "Enter the tableau column index: " in
          let col_index = read_int () in
          Game.move_matching_card_to_tableau g col_index
      | _ -> failwith "Incomplete Validation Function"
    in
    print_error error;
    (true, g2)

    