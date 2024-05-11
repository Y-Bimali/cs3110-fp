val round : Game.t -> bool * Game.t
(** [round g] prints [g], queries the player, and is [g2, b], where [g2] is the
    (possibly unchanged from [g]) game resulting from the player's response, and
    [b] indicating that the player will continue playing (i.e. false only if
    they want to quit). [g2] = [g] for any unrecognized response, or any
    response that is not associated with gameplay, e.g. "help" or setting a
    theme. When called for the first time, the "Classic" color theme is used,
    and the board is printed without indices. These preferences may change
    through the player's response to queries. *)

val help_str : string
(** [help_str] is the string of instructions to interact with the game.*)
