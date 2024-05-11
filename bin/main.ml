(* @author Aruni Chenxi (ac2552), Malli Gutta (mg2395), Yinghui Bimali (yb272),
   Desmond Ababio (dna35)*)
open Fp.Game
open Fp.Printer

(** [game_loop g] is a new loop of the game starting with game [g].*)
let rec game_loop g =
  let b, g2 = round g in
  match b with
  | true -> game_loop g2
  | false -> ()

let () = Random.self_init ()

let instructions () =
  print_endline "";
  print_endline "This is a demo of a Klondike Solitaire game.";
  print_endline help_str

let _ =
  instructions ();
  game_loop (new_game ())
