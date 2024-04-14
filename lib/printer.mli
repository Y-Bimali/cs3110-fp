(* val print_game : Game.t -> unit (** [print_game g] prints out a formatted [g]
   to the terminal*)

   val process_move : Game.t -> string -> Game.t (** If there is an error, it
   will print it out and return the same game. If valid move, it will return
   updated game*) *)
type theme

val dt : theme

val round : Game.t -> bool * Game.t
(** [round g] queries the player and returns the game after their next valid
    move (or the same game if they opt to quit). [b] indicates if they made a
    move or created a new game, and false means they would like to quit. Default
    color scheme is used.*)