type cell =
  | Wall
  | Movable
  | Empty 

type t

(**[empty_t] returns empty t *)
val empty_maze_t: t 

(** [parse_map_file filename] is the constructed [map] after parsing 
    [filename]. *)
val maze_from_file : string -> t 

(**[print_maze_t t] prints a representation of the maze in terminal*)
val print_maze : t -> string
