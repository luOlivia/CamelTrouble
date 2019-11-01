type t

type pos = {
  mutable x: float;
  mutable y: float;
  mutable angle_degrees: float;
}

type ball_action =
  | CollisionCamel 
  | CollisionWall
  | BallMoving