open Camel 

(** The type [t] represents the state of the game *)
type t = {
  ball_list: Ball.t list;
  camel1 : Camel.t;
  camel2 : Camel.t;
  camel1_alive : bool;
  camel2_alive : bool; 
  maze: Maze.t
} 

(** [camel_width] is width of the camel. *)
val camel_width : float
(** [ball_width] is width of the ball. *)
val ball_width : float 
(** [wall_width] is width of the wall. *)
val wall_width : float
(** [wall_height] is height of the wall. *)
val wall_height : float
(** [square_width] is width of the grid squares. *)
val square_width : float 

(**[move_ball st ball] is the new [ball] with new pos 
   and angle after moving one step *)
val move_ball: t -> Ball.t -> Ball.t 

(** [move_fwd st camel] is [camel] with new pos after moving forwards. *)
val move_fwd : t -> Camel.t -> Camel.t
(** [move_rev st camel] is [camel] with new pos after moving backwards. *)
val move_rev : t -> Camel.t -> Camel.t