(** [player_num] represents a single of the two possible players in the game *)
type player_num = One | Two

(** The type [Camel.t] represents a camel *)
type t = {
  (** [player_num] is the player controlling [Camel.t] *)
  player_num: player_num;
  (** [player_name] is the player name of [Camel.t] *)
  player_name: string;
  (** [score] is the points of [Camel.t] *)
  score : int; 
  (** [num_balls] is the number of balls currently out of [Camel.t] *)
  num_balls: int;
  (** [dir] is the angle pointed by [Camel.t], and is in [[0.0, 360.0)] *)
  dir: float; 
  (** [pos] is the position of [Camel.t] *)
  pos: Position.t;
  (** [shot_time] is the last time [Camel.t] shot a ball *)
  shot_time: float; 
}

(** [init num x y score name] is initialized Camel before gameplay *)
val init : player_num -> float -> float -> int -> string -> t

(** [fwd_speed] is speed the tank moves forward *)
val fwd_speed : float
(** [rev_speed] is speed the tank moves backward *)
val rev_speed : float

(** [turn_right camel] is [camel] with right rotation *)
val turn_right : t -> t

(** [turn_left camel] is [camel] with left rotation *)
val turn_left : t -> t

(** [move_horiz x dir speed] is the position after moving 
    horizontally from [x] with [speed] *)
val move_horiz : float -> float -> float -> float

(** [move_vert y dir speed] is the position after 
    moving vertically from [y] with [speed] *)
val move_vert : float -> float -> float -> float

(** [free_range camel speed] is [camel] moved without regard to walls *)
val free_range : t -> float -> t

(** [to_str camel] is string representation of [camel] *)
val to_str : t -> string