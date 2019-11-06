type t = {
  name: string;
  num_bullets: int;
  dir: float; (* degrees 0 - 360 *)
  pos: Position.t
}

val fwd_speed : float
val rev_speed : float

val move_horiz : float -> float -> float -> float
val move_vert : float -> float -> float -> float

val hit : t -> t 