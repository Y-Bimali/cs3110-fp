(* @author *)
open Fp.Game
open Fp.Printer

let rec game_loop g =
  let () = print_game g in
  let move = read_line () in
  game_loop (process_move g move)

let _ = game_loop new_game
