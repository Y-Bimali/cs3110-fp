(* @author *)
open Fp.Game
open Fp.Printer

let counter = ref 0

let rec game_loop theme g t =
  let b, t2, g2 = round theme g counter t in
  match b with
  | true -> game_loop t2 g2 t
  | false -> ()

let () = Random.self_init ()

let instructions () =
  print_endline "";
  print_endline "This is a demo of a Klondike Solitaire game.";
  print_endline help_str

let _ =
  (* print_endline ("Number of valid moves: " ^ string_of_int !counter); *)
  instructions ();
  game_loop Classic (new_game ()) (Unix.gettimeofday ())
