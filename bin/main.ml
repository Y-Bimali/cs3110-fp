(* @author *)
open Fp.Game
open Fp.Printer

let rec game_loop g =
  let b, g2 = round g in
  match b with
  | true -> game_loop g2
  | false -> ()

let () = Random.self_init ()

let instructions () =
  print_endline "";
  print_endline
    "This is a demo of a Klondike Solitaire game. Supported commands are \
     listed below. Commands are case insensitive.";
  print_endline "Type \"New Game\" to start a new game.";
  print_endline
    "Type \"Draw\" or \"D\" to draw a card from the stock to the waste, or \
     return all cards in the waste back to the stock. The next draw will flip \
     cards from top to bottom again.";
  print_endline "Type \"Quit\" to quit the program."

let _ =
  instructions ();
  game_loop (new_game ())
