type cell =
  | Wall
  | Movable
  | Empty 

type t = cell array array

let parse_map_file filename =
  failwith "unimplemented"