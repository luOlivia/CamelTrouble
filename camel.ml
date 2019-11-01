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
  | Shoot

type camel_action =
  | Shooting 
  | CamelMoving
  | Rotating
  | Stationary

type t = {
  position: pos; 
  curr_control: controls; 
  curr_action: camel_action; 
  is_alive: bool
}

let camel_init pos =
  {position=pos; curr_control=Left; curr_action=Stationary; is_alive=true}

(* let move_camel control camel_action camel =
   match control with
   | Left ->  *)
