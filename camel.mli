type t

type pos = {
  mutable x: float;
  mutable y: float;
  mutable angle_degrees: float;
}

type controls =
  | Left
  | Right
  | Up
  | Down

type camel_action =
  | Shooting 
  | CamelMoving
  | Rotating
