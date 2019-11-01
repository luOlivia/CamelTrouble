type pos = {
  mutable x: float;
  mutable y: float;
}

type ball_action =
  | CollisionCamel 
  | CollisionWall
  | BallMoving