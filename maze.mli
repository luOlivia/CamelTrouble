type wall = Horizontal of bool array array | Vertical of bool array array

type t = {horizontal_walls: wall; vertical_walls: wall}

val num_grid_squares : int
val make_maze : int -> t

val is_wall_left : t -> int -> int -> bool
val is_wall_right : t -> int -> int -> bool
val is_wall_above : t -> int -> int -> bool
val is_wall_below : t -> int -> int -> bool

val to_str : t -> string