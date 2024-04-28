(* @author *)
open Fp.Game
open Fp.Printer

let rec game_loop theme g =
  let b, t2, g2 = round theme g in
  match b with
  | true -> game_loop t2 g2
  | false -> ()

let () = Random.self_init ()

let instructions () =
  print_endline "";
  print_endline "This is a demo of a Klondike Solitaire game.";
  print_endline help_str

let _ =
  instructions ();
  game_loop Classic (new_game ())
