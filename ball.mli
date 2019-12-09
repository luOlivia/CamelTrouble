(** [Ball.t] is a represents a shot ball as projectile *)
type t = 
  {
    (** [timer] is the remaining time of a [Ball.t] *)
    timer: float;
    (** [owner] is the Camel who shot a [Ball.t] *)
    owner: Camel.t;
    (** [angle] is the current angle in degrees at which a [Ball.t] is moving *)
    angle: float;
    (** [position] is the current position at which a [Ball.t] is *)
    position: Position.t;
    (** [speed] controls how fast a [Ball.t] moves *)
    speed: float;
  }

(** [init owner angle x y] is a new ball shot by [owner] at a given [angle] at 
    position [x] [y] *)
val init : Camel.t -> float -> float -> float -> t

(** [new_ball_pos_x b] is the x-position of the ball [b] with a movement 
    step in the x-direction *)
val new_pos_x : t -> float 

(** [new_ball_pos_y b] is the y-position of the ball [b] with a movement 
    step in the y-direction *)
val new_pos_y : t -> float 

(** [flip_ball_h b] is the ball [b] flipped in its horizontal movement *)
val flip_h : t -> t 

(** [flip_ball_v b] is the ball [b] flipped in its vertical movement *)
val flip_v : t -> t 

(** [step_timer b] is the ball [b] with its timer decremented *)
val step_timer : t -> t
