open Camel 

type t = {
  ball_list: Ball.t list;
  camel1 : Camel.t;
  camel2 : Camel.t;
  camel1_alive : bool;
  camel2_alive : bool; 
  maze: Maze.t
} 

val camel_width : float

(** [ball_width] is width of the ball.*)
val ball_width : float 

val wall_width : float

val square_width : float 