type pos = {
  mutable x: float;
  mutable y: float;
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