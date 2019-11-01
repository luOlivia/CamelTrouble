type t

val init_state : Maze.t -> Camel.t list option -> t

val update_state : t -> Camel.t list option -> Ball.t list option -> t