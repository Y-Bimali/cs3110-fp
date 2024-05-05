(* @author *)
open Fp.Game
open Fp.Printer

let rec game_loop g t =
  let b, g2 = round g t in
  match b with
  | true -> game_loop g2 t
  | false -> ()

let () = Random.self_init ()

let instructions () =
  print_endline "";
  print_endline "This is a demo of a Klondike Solitaire game.";
  print_endline help_str

let _ =
  (* print_endline ("Number of valid moves: " ^ string_of_int !counter); *)
  instructions ();
  game_loop (new_game ()) (Unix.gettimeofday ())
