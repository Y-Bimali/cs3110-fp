val round : Game.t -> bool * Game.t
(** [round g] queries the player and returns the game after their next valid
    move (or the same game if they opt to quit). [b] indicates if they made a
    move or created a new game, and false means they would like to quit. Default
    color scheme is used.*)
(*TODO: Bimali change this*)

val help_str : string
(** [help_str] is the string of instructions to interact with the game.*)
