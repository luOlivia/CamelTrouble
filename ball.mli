type t = {
  timer: float;
  owner: Camel.t;
  angle: float;
  position: Position.t;
  ballspeed: float;
}

val init: Camel.t -> float -> float -> float -> t

val new_ball_pos_x: t -> float 

val new_ball_pos_y: t -> float 

val flip_ball_h: t -> t 

val flip_ball_v: t -> t 

val get_position: t -> Position.t

val get_angle: t -> float 

val step_timer : t -> t
