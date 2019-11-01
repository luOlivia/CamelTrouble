type cell =
  | Wall
  | Movable
  | Empty 

type t

(** [parse_map_file filename] is the constructed [map] after parsing 
    [filename]. *)
val maze_from_file : string -> t 