(** [wall] of booleans representing the presence of walls. 
    A [cell x y] can have walls:
    - above [Horizontal (y)(x)]
    - below [Horizontal (y+1)(x)]
    - left  [Vertical (x)(y)]
    - below [Vertical (x+1)(y)]
*)
type wall = 
  | Horizontal of bool array array 
  | Vertical of bool array array

(**[t] is a record of all horizontal and vertical walls in the maze.*)
type t = {horizontal_walls: wall; vertical_walls: wall}

(**[num_grid_squares] is the number of spaces between walls in a maze row*)
val num_grid_squares : int

(**[density] is a parameter determining the wall density of generated mazes *)
val density : int

(**[maze_maze density] creates a maze using a given wall [density]*)
val make_maze : int -> t

(**[is_wall_left maze x y] is [true] if there is a wall on the left in 
   the grid square represented by x and y, and [false] otherwise *)
val is_wall_left : t -> int -> int -> bool

(**[is_wall_right maze x y] is [true] if there is a wall on the right in 
   the grid square represented by x and y, and [false] otherwise *)
val is_wall_right : t -> int -> int -> bool

(**[is_wall_above maze x y] is [true] if there is a wall on the top of
   the grid square represented by x and y, and [false] otherwise *)
val is_wall_above : t -> int -> int -> bool

(**[is_wall_below maze x y] is [true] if there is a wall on the bottom of 
   the grid square represented by x and y, and [false] otherwise *)
val is_wall_below : t -> int -> int -> bool

(**[to_str maze] is a string representation of the [maze]*)
val to_str : t -> string