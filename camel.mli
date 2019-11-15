type t = {
  name: string;
  num_bullets: int;
  dir: float; (* degrees 0 - 360 *)
  pos: Position.t
}

val init : string -> float -> float -> t
val fwd_speed : float
val rev_speed : float

val turn_right : t -> t
val turn_left : t -> t

val move_horiz : float -> float -> float -> float
val move_vert : float -> float -> float -> float

val free_range : t -> float -> t

(* val move_fwd : t -> t
   val move_rev : t -> t *)

val hit : t -> t 

val to_str : t -> string