type cell =
  | Wall
  | Movable
  | Empty 

type map = 
  cell array array

(** [parse_map_file filename] is the constructed [map] after parsing 
    [filename]. *)
val parse_map_file : string -> map