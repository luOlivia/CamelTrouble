
(** [status] represents the current status that the game is in for the user *)
type status = Start | Playing | Paused 

(** The type [t] represents the state of the game *)
type t = {
  ball_list: Ball.t list;
  camel1 : Camel.t;
  camel2 : Camel.t;
  camel1_alive : bool;
  camel2_alive : bool; 
  game_end : bool;
  maze: Maze.t;
  status: status
} 

(** [movement] represents the direction that the Camel moves in *)
type movement = Forward | Reverse

(** [rotation] represents the direction that the Camel rotates towards *)
type rotation = CounterClockwise | Clockwise

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

(**[xDimension] and [yDimension] make up the dimensions of the play screen in pixels*)
val xDimension : int 
val yDimension : int

(**[move_ball st ball] is the new [ball] with new pos 
   and angle after moving one step *)
val move_ball: t -> Ball.t -> Ball.t 

(** [move_fwd st camel] is [camel] with new pos after moving forwards. *)
val move_fwd_collide : t -> Camel.t -> Camel.t

(** [move_rev st camel] is [camel] with new pos after moving backwards. *)
val move_rev_collide : t -> Camel.t -> Camel.t

(**[shoot camel st] is the new [st] after [camel] has shot a ball*)
val shoot : Camel.t -> t -> t

(**[move direction st camel] is the new [st] with [camel] position moved*)
val move : movement -> t -> Camel.t -> t

(**[rotate d st camel] is the new [st] after rotating [camel] in direction [d]*)
val rotate : rotation -> t -> Camel.t -> t

(** [rot_point x y center_x center_y angle] is the point [x] and [y] rotated 
    around center [center_x] [center_y]pt by [angle] *)
val rot_point : float -> float -> float -> float -> float -> int * int

(**[update_state state] is the new [state] after moving all balls*)
val update_state : t -> t

(**[init_state] is the initial state*)
val init_state : t