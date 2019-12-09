(** [Cell.t] is a represents a cell in maze *)
type t = {x: int; y: int}

(** [init x y] is a new cell at position [x] [y] *)
val init : int -> int -> t