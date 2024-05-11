(** AF/RI: None. This module does not have an associated type.*)

let help_str =
  BatEnum.fold
    (fun acc x -> acc ^ "\n" ^ x)
    ""
    (BatFile.lines_of "data/help.txt")

(** [!theme] Determines the colors that different parts of the card/board will
    be printed with.*)
let theme = ref (Theme.theme_of_string "classic")

(** [!indices] Is true if the board will be printed with indices above the
    foundation/tableau, and false if not.*)
let indices = ref false

(* * * * *)
(* A few utility functions common to multiple sections. *)
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
    representation if it is only two characters long (i.e. not of rank 10), or
    returns [c]'s string representation otherwise.*)
let reformat_card c =
  let s = Card.to_string c in
  if Card.num_of c <> 10 then " " ^ s else s

let blank x = String.make x ' '
let back_blank x = ([ !theme.bg ], blank x)
let blank_row () = back_blank 49
let norm_card_strings c = [ reformat_card c ^ "  "; " XXX "; " XXX "; "     " ]
let left_card_strings c = [ reformat_card c ^ "|"; " XX|"; " XX|"; "   |" ]

let color c =
  match Card.color_of c with
  | Card.Black -> !theme.black
  | Card.Red -> !theme.red

let card_tagged style c =
  let form =
    match style with
    | Top | Full | Left -> [ color c; !theme.faces ]
    | NoneFull | NoneLeft | NoneTop -> [ !theme.bg ]
    | Back | BackTop -> [ !theme.back_pattern; !theme.backs ]
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

(* * * * *)
(* The following section implements the mechanics of printing the top bar
   (stock, waste, and foundation) of the board. *)

let top_indices () =
  let x = ref (blank 22) in
  for i = 1 to 4 do
    x := !x ^ blank 5 ^ string_of_int i
  done;
  ([ !theme.bg; !theme.text ], !x ^ blank 3)

let stock_app s =
  if s then card_tagged NoneFull (Card.empty_card Card.Spades)
  else card_tagged Back (Card.empty_card Card.Spades)

let tag_change_0 emp =
  transpose
    [
      card_tagged NoneLeft emp;
      card_tagged NoneLeft emp;
      card_tagged NoneFull emp;
    ]

let tag_change_1 emp w =
  transpose
    [
      card_tagged NoneLeft emp;
      card_tagged NoneLeft emp;
      card_tagged Full (List.nth w 0);
    ]

let tag_change_2 emp w =
  transpose
    [
      card_tagged NoneLeft emp;
      card_tagged Left (List.nth w 1);
      card_tagged Full (List.nth w 0);
    ]

let tag_change_other w =
  transpose
    [
      card_tagged Left (List.nth w 2);
      card_tagged Left (List.nth w 1);
      card_tagged Full (List.nth w 0);
    ]

let waste_app w =
  let emp = Card.empty_card Card.Spades in
  match List.length w with
  | 0 -> tag_change_0 emp
  | 1 -> tag_change_1 emp w
  | 2 -> tag_change_2 emp w
  | _ -> tag_change_other w

let found_app_mod c =
  if Card.num_of c = 0 then card_tagged NoneFull c else card_tagged Full c

let found_app f = transpose (List.map found_app_mod f)

let top_help_helper bb hs hw hf =
  [
    bb;
    hs;
    bb;
    List.nth hw 0;
    List.nth hw 1;
    List.nth hw 2;
    back_blank 5;
    List.nth hf 0;
    bb;
    List.nth hf 1;
    bb;
    List.nth hf 2;
    bb;
    List.nth hf 3;
    bb;
  ]

let rec top_help lst s w_lst f_lst =
  match (s, w_lst, f_lst) with
  | [], [], [] -> lst
  | hs :: ts, hw :: tw, hf :: tf ->
      let bb = back_blank 1 in
      top_help (lst @ [ top_help_helper bb hs hw hf ]) ts tw tf
  | _ -> failwith "Incompatible Formats"

(** [format_top s w f] returns a print-ready representation of the top bar of
    the board (stock, waste, and foundation), with a margin above and below. *)
let format_top s w f =
  [ [ blank_row () ] ]
  @ (if !indices then [ [ top_indices () ] ] else [])
  @ top_help [] (stock_app s) (waste_app w) (found_app f)
  @ [ [ blank_row () ] ]

let print_top g =
  match Game.formatted g with
  | (s, w), f, _ -> print_board (format_top s w f)

(* * * * *)
(* The following section implements the mechanics of printing the tableau. *)

let tab_indices () =
  let x = ref (blank 3) in
  for i = 1 to 6 do
    x := !x ^ string_of_int i ^ blank 6
  done;
  ([ !theme.bg; !theme.text ], !x ^ "7" ^ blank 3)

(** [list_collapse lst] is the list resulting from collapsing the elements of a
    list of lists into a single list*)
let list_collapse lst = List.fold_left (fun acc x -> x @ acc) [] (List.rev lst)

let rec pad_out rem padding out_lst in_lst =
  if rem = 0 then List.rev out_lst
  else
    match in_lst with
    | [] -> pad_out (rem - 1) padding (padding :: out_lst) []
    | h :: t -> pad_out (rem - 1) padding (h :: out_lst) t

let justify lsts padding =
  let len = List.fold_left (fun acc lst -> max acc (List.length lst)) 0 lsts in
  List.map (pad_out len padding []) lsts

let tab_app_mod booled_c =
  match booled_c with
  | last, c ->
      if last then card_tagged Full c
      else if Card.num_of c = 0 then card_tagged BackTop c
      else card_tagged Top c

let tab_app booled_t =
  transpose
    (justify
       (List.map (fun x -> list_collapse (List.map tab_app_mod x)) booled_t)
       (List.hd (card_tagged NoneTop (Card.empty_card Card.Spades))))

(** [alternate sep [] in_lst] is a list that alternates the values of [in_lst]
    with [sep], for a final list with the n elements of [in_lst] and n-1
    elements being [sep] (or an empty list if [in_lst] is empty).*)
let rec alternate sep out_lst in_lst =
  match in_lst with
  | [] -> []
  | a :: [] -> List.rev (a :: out_lst)
  | h :: t -> alternate sep (sep :: h :: out_lst) t

let rec tab_bool_tags_helper out_lst in_lst =
  match in_lst with
  | [] -> []
  | h :: [] -> List.rev ((true, h) :: out_lst)
  | h :: t -> tab_bool_tags_helper ((false, h) :: out_lst) t

(** [tab_bool_tags col] is a list of tuples containing true, whether the card is
    the last card in the column, and the card, given a column of (top to bottom)
    cards in the tableau *)
let tab_bool_tags col = tab_bool_tags_helper [] col

let format_tab t =
  let flipped_t = List.map List.rev t in
  let booled_t = List.map tab_bool_tags flipped_t in

  (if !indices then [ [ tab_indices () ] ] else [])
  @ List.map
      (fun x ->
        (back_blank 1 :: alternate (back_blank 2) [] x) @ (back_blank 1 :: []))
      (tab_app booled_t)
  @ [ [ blank_row () ] ]

let print_tab g =
  match Game.formatted g with
  | _, _, t -> print_board (format_tab t)

(* * * * *)
(* The following section implements the round functionality of the printer. *)

let rules_str =
  BatEnum.fold
    (fun acc x -> acc ^ "\n" ^ x)
    ""
    (BatFile.lines_of "data/rules.txt")

let print_error e =
  match e with
  | None -> ()
  | Some s -> print_endline s

let remove_excess_whitespace str =
  let rec trim_space acc = function
    | [] -> List.rev acc
    | "" :: tl -> trim_space acc tl
    | hd :: tl -> trim_space (hd :: acc) tl
  in
  let words = String.split_on_char ' ' str in
  let trimmed_words = trim_space [] words in
  String.concat " " trimmed_words

let slice_from_index_to_end str index =
  String.sub str index (String.length str - index)

let winning_statement g =
  "\nCongratulations! You won the game in "
  ^ string_of_int (Game.get_count g)
  ^ " valid moves without undos, and in "
  ^ string_of_int (Game.get_count g + Game.get_undos g)
  ^ " moves including undos.\nYour total time spent was "
  ^ string_of_int (int_of_float (Unix.gettimeofday () -. Game.start_time g))
  ^ " seconds.\nType 'New Game' to play a new game."

let display_theme g q =
  (theme :=
     try Theme.theme_of_string (slice_from_index_to_end q 6)
     with Theme.UnknownTheme ->
       print_endline
         "Unrecognizable Theme. Enter \"help\" to view the list of themes.";
       !theme);
  (true, g)

let foundation_to_tableau_helper g x y =
  let f_int = int_of_string (slice_from_index_to_end x 1) in
  let t_int = int_of_string (slice_from_index_to_end y 1) in
  let f_index = if f_int >= 0 then f_int - 1 else f_int in
  let t_index = if t_int >= 0 then t_int - 1 else t_int in

  Game.move_card_from_foundation_to_tableau g f_index t_index

let tableau_to_tableau_helper g x y =
  let c1 = slice_from_index_to_end x 1 in
  let c2 = slice_from_index_to_end y 1 in
  Game.t_to_t g c1 c2

let tableau_to_foundation_helper g x =
  let col_index = slice_from_index_to_end x 1 in

  Game.move_tableau_card_to_foundation g (int_of_string col_index - 1)

let game_ended_str =
  "The game has ended. Type \"new game\" to start a new game or quit to end \
   the game!"

let check_conditions_for_three_word_commands g x y =
  if not (Game.check_win g) then
    if String.get x 0 = 'f' && String.get y 0 = 't' then
      foundation_to_tableau_helper g x y
    else if String.get x 0 = 't' && String.get y 0 = 't' then
      tableau_to_tableau_helper g x y
    else if String.get x 0 = 't' && String.get y 0 = 'f' && String.length y = 1
    then tableau_to_foundation_helper g x
    else if
      (String.get x 0 = 's' && String.length x = 1) && String.get y 0 = 't'
    then
      let tab_index = slice_from_index_to_end y 1 in
      Game.s_to_t g (int_of_string tab_index - 1)
    else (g, Some "Invalid action.")
  else (g, Some game_ended_str)

let invalid_command_str =
  "The command is not valid. Enter \"help\" for a list of valid commands."

let autowin_animation g =
  List.fold_left
    (fun _ newg ->
      let () = print_top newg in
      print_tab newg;
      print_endline "";
      Unix.sleepf 0.5)
    () (Game.autowin_gamelist g)

(* These commands should only be used during a game.*)
let match_other other g =
  match other with
  | "draw" | "d" -> Game.draw_card g
  | "autowin" ->
      let out = Game.autowin g in
      if snd out = None then autowin_animation g;
      out
  | "undo" -> Game.undo g
  | str -> (
      let v = String.split_on_char ' ' str in
      try
        match v with
        | [ "s"; "to"; "f" ] -> Game.s_to_f g
        | [ x; "to"; y ] -> check_conditions_for_three_word_commands g x y
        | [ "cheat"; x; y ] -> Game.cheat g x y
        | _ -> (g, Some invalid_command_str)
      with Failure _ -> (g, Some invalid_command_str))

(* These commands are valid even if the game has ended.*)
let match_statements q g =
  match q with
  | "count" ->
      ( g,
        Some
          ("Moves: "
          ^ string_of_int (Game.get_count g)
          ^ "\nMoves including Undos: "
          ^ string_of_int (Game.get_count g + Game.get_undos g)) )
  | "help" | "commands" -> (g, Some help_str)
  | "rules" -> (g, Some rules_str)
  | "index" ->
      indices := not !indices;
      (g, None)
  | "new game" -> (Game.new_game (), None)
  | "new game 3" -> (Game.update_three_opt (Some "3") (Game.new_game ()), None)
  | other ->
      if Game.check_win g then (g, Some game_ended_str) else match_other other g

(** The round function leaves other functions to parse most possible commands
    (see [match_statements] and [match_other]), with the exception of "quit" and
    "theme_".*)
let round g =
  let () = print_top g in
  print_tab g;
  print_endline "";
  print_string "Enter an action: ";
  let has_won_alr = Game.check_win g in
  let q = remove_excess_whitespace (String.lowercase_ascii (read_line ())) in
  if q = "quit" then (false, g)
  else if String.starts_with ~prefix:"theme" q then display_theme g q
  else
    let g2, error = match_statements q g in
    if Game.check_win g2 && not has_won_alr then
      print_endline (winning_statement g2);
    print_error error;
    (true, g2)
