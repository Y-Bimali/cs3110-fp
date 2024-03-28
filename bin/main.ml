(* @author *)
open Fp.Game
open Fp.Printer

let rec game_loop g =
  let b, g2 = round g in
  match b with
  | true -> game_loop g2
  | false -> ()

let () = Random.self_init ()
let _ = game_loop (new_game ())
