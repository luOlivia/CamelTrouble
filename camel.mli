(** [player_num] represents a single of the two possible players in the game *)
type player_num = One | Two

(** The type [Camel.t] represents a camel *)
type t = {
  player_num: player_num;
  player_name: string;
  score : int; 
  num_balls: int;
  dir: float; (* degrees 0 - 360 *)
  pos: Position.t;
  shot_time: float; 
}

(** [init num x y score name] is initialized camel before gameplay *)
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