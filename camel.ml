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

type t = {
  position: pos; 
  curr_control: controls; 
  curr_action: camel_action; 
  is_alive: bool
}


