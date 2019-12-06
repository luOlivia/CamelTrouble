(** The type [t] represents a camel *)
type t = {
  player_num: int;
  num_bullets: int;
  dir: float; (* degrees 0 - 360 *)
  pos: Position.t;
  shot_time: float; 
}

(** [init num x y] is initialized camel before gameplay *)
val init : int -> float -> float -> t

(** [fwd_speed] is speed the tank moves forward *)
val fwd_speed : float
(** [rev_speed] is speed the tank moves backward *)
val rev_speed : float

(** [turn_right camel] is [camel] with right rotation *)
val turn_right : t -> t

(** [turn_left camel] is [camel] with left rotation *)
val turn_left : t -> t

(** [move_horiz x dir] is [x] pos after moving horizontally *)
val move_horiz : float -> float -> float -> float

(** [move_vert y dir] is [y] pos after moving vertically *)
val move_vert : float -> float -> float -> float

(** [free_range camel y dir] is [camel] moved w/o regard to walls *)
val free_range : t -> float -> t

(** [hit camel] is whether [camel] is alive *)
(* val hit : t -> bool *)

(** [to_str camel] is string rep of [camel] params *)
val to_str : t -> string