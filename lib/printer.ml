let help_str =
  BatEnum.fold
    (fun acc x -> acc ^ "\n" ^ x)
    ""
    (BatFile.lines_of "data/help.txt")

module MakePrinter (T : Theme.T) = struct
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
      representation if it is only two characters long, or returns [c]'s string
      representation otherwise.*)
  let reformat_card c =
    let s = Card.to_string c in
    if Card.num_of c <> 10 then " " ^ s else s

  let blank x = String.make x (String.get " " 0)
  let back_blank x = ([ T.bg ], blank x)
  let blank_row = back_blank 49

  let norm_card_strings c =
    [ reformat_card c ^ "  "; " XXX "; " XXX "; "     " ]

  let left_card_strings c = [ reformat_card c ^ "|"; " XX|"; " XX|"; "   |" ]

  let color c =
    match Card.color_of c with
    | Card.Black -> T.black
    | Card.Red -> T.red

  let card_tagged style c =
    let form =
      match style with
      | Top | Full | Left -> [ color c; T.faces ]
      | NoneFull | NoneLeft | NoneTop -> [ T.bg ]
      | Back | BackTop -> [ T.back_pattern; T.backs ]
    in
    match style with
    | Top -> [ (form, reformat_card c ^ "  ") ]
    | Full -> List.map (fun x -> (form, x)) (norm_card_strings c)
    | Left -> List.map (fun x -> (form, x)) (left_card_strings c)
    | NoneFull -> List.map (fun x -> (form, x)) (List.init 4 (fun _ -> blank 5))
    | NoneLeft -> List.map (fun x -> (form, x)) (List.init 4 (fun _ -> blank 4))
    | NoneTop -> [ (form, blank 5) ]
    | Back ->
        List.map (fun x -> (form, x)) [ "XXXXX"; "X###X"; "X###X"; "XXXXX" ]
    | BackTop -> [ (form, "XXXXX") ]

  (* * * * *)
  (* The following section implements the mechanics of printing the top bar
     (stock, waste, and foundation) of the board. *)

  let stock_app s =
    if s then card_tagged NoneFull (Card.empty_card Card.Spades)
    else card_tagged Back (Card.empty_card Card.Spades)

  let waste_app w =
    let emp = Card.empty_card Card.Spades in
    match List.length w with
    | 0 ->
        transpose
          [
            card_tagged NoneLeft emp;
            card_tagged NoneLeft emp;
            card_tagged NoneFull emp;
          ]
    | 1 ->
        transpose
          [
            card_tagged NoneLeft emp;
            card_tagged NoneLeft emp;
            card_tagged Full (List.nth w 0);
          ]
    | 2 ->
        transpose
          [
            card_tagged NoneLeft emp;
            card_tagged Left (List.nth w 1);
            card_tagged Full (List.nth w 0);
          ]
    | _ ->
        transpose
          [
            card_tagged Left (List.nth w 2);
            card_tagged Left (List.nth w 1);
            card_tagged Full (List.nth w 0);
          ]

  let found_app_mod c =
    if Card.num_of c = 0 then card_tagged NoneFull c else card_tagged Full c

  let found_app f = transpose (List.map found_app_mod f)

  let rec top_help lst s w_lst f_lst =
    match (s, w_lst, f_lst) with
    | [], [], [] -> lst
    | hs :: ts, hw :: tw, hf :: tf ->
        let bb = back_blank 1 in
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
                back_blank 5;
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
          ts tw tf
    | _ -> failwith "Incompatible Formats"

  (** [format_top s w f] returns a print-ready representation of the top bar of
      the board (stock, waste, and foundation), with a margin above and below. *)
  let format_top s w f =
    [ [ blank_row ] ]
    @ top_help [] (stock_app s) (waste_app w) (found_app f)
    @ [ [ blank_row ] ]

  let print_top g =
    match Game.formatted g with
    | (s, w), f, _ -> print_board (format_top s w f)

  (* * * * *)
  (* The following section implements the mechanics of printing the tabelau. *)

  (** [list_collapse lst] is the list resulting from collapsing the elements of
      a list of lists into a single list*)
  let list_collapse lst =
    List.fold_left (fun acc x -> x @ acc) [] (List.rev lst)

  let rec pad_out rem padding out_lst in_lst =
    if rem = 0 then List.rev out_lst
    else
      match in_lst with
      | [] -> pad_out (rem - 1) padding (padding :: out_lst) []
      | h :: t -> pad_out (rem - 1) padding (h :: out_lst) t

  let justify lsts padding =
    let len =
      List.fold_left (fun acc lst -> max acc (List.length lst)) 0 lsts
    in
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

  (** [tab_bool_tags col] is a list of tuples containing true, whether the card
      is the last card in the column, and the card, given a column of (top to
      bottom) cards in the tableau *)
  let tab_bool_tags col = tab_bool_tags_helper [] col

  let format_tab t =
    let flipped_t = List.map List.rev t in
    let booled_t = List.map tab_bool_tags flipped_t in
    List.map
      (fun x ->
        (back_blank 1 :: alternate (back_blank 2) [] x) @ (back_blank 1 :: []))
      (tab_app booled_t)
    @ [ [ blank_row ] ]

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

  (*TODO: output tuple should be bool. Game Functions called in Game should
    return game * string option *)

  let round theme g c =
    let () = print_top g in
    print_tab g;
    print_endline "";
    print_string "Enter an action: ";
    let q = remove_excess_whitespace (String.lowercase_ascii (read_line ())) in
    if q = "quit" then (false, theme, g)
    else if String.starts_with ~prefix:"theme" q then (
      try (true, Theme.theme_of_string (String.sub q 6 (String.length q - 6)), g)
      with Theme.UnknownTheme ->
        print_endline
          "Unrecognizable Theme. Enter \"help\" for a list of themes.";
        (true, theme, g))
    else
      let g2, error =
        match q with
        | "help" | "commands" -> (g, Some help_str)
        | "rules" -> (g, Some rules_str)
        | "draw" | "d" -> Game.draw_card g c
        | "new game" -> (Game.new_game (), None)
        | str -> (
            let v = String.split_on_char ' ' str in
            try
              match v with
              | [ "s"; "to"; "f" ] -> Game.s_to_f g c
              | [ x; "to"; y ] ->
                  if String.get x 0 = 'f' && String.get y 0 = 't' then
                    let f_index = slice_from_index_to_end x 1 in
                    let t_index = slice_from_index_to_end y 1 in

                    Game.move_card_from_foundation_to_tableau g
                      (int_of_string f_index - 1)
                      (int_of_string t_index - 1)
                      c
                  else if String.get x 0 = 't' && String.get y 0 = 't' then
                    let c1 = slice_from_index_to_end x 1 in
                    let c2 = slice_from_index_to_end y 1 in

                    Game.t_to_t g c1 c2 "1" c
                  else if
                    String.get x 0 = 't'
                    && String.get y 0 = 'f'
                    && String.length y = 1
                  then
                    let col_index = slice_from_index_to_end x 1 in

                    Game.move_tableau_card_to_foundation g
                      (int_of_string col_index - 1)
                      c
                  else if
                    (String.get x 0 = 's' && String.length x = 1)
                    && String.get y 0 = 't'
                  then
                    let tab_index = slice_from_index_to_end y 1 in

                    Game.s_to_t g (int_of_string tab_index - 1) c
                  else (g, Some "Invalid action.")
              | [ x; i; "to"; y ] ->
                  if String.get x 0 = 't' && String.get y 0 = 't' then
                    let c1 = slice_from_index_to_end x 1 in
                    let c2 = slice_from_index_to_end y 1 in

                    Game.t_to_t g c1 c2 i c
                  else (g, Some "Invalid command.")
              | _ -> (g, Some "Invalid command.")
            with Failure _ -> (g, Some "The last command is not valid."))
      in
      if Game.check_win g2 then
        print_endline "You win! Type 'New Game' to play a new game.";
      print_error error;

      print_endline ("Number of valid moves: " ^ string_of_int !c);
      (true, theme, g2)
end

let round theme g c =
  match theme with
  | Theme.Classic ->
      let module Current = MakePrinter (Theme.MClassic) in
      Current.round theme g c
  | Theme.Spaceship ->
      let module Current = MakePrinter (Theme.MSpaceship) in
      Current.round theme g c
  | Theme.UnderTheSea ->
      let module Current = MakePrinter (Theme.MUnderTheSea) in
      Current.round theme g c
