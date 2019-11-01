type pos = {
  mutable x: float;
  mutable y: float;
  mutable angle_degrees: float;
}

type ball_action =
  | CollisionCamel 
  | CollisionWall
  | BallMoving

type t = {
  position: pos; 
  curr_action: ball_action; 
  time_left: float
}

let ball_init pos =
  {position=pos; curr_action=BallMoving; time_left=1000.0}
