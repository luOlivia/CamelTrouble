open Position
open Camel  
(* 
- timer 
- owner 
- angle 
- position (type point)
- ballspeed 
- engine 
*)

type t = 
  {
    timer: float;
    owner: Camel.t;
    angle: float;
    position: Position.t;
    ballspeed: float;
  }

let init owner a x y = 
  {
    timer= 10.0;
    owner= owner;
    angle= a;
    ballspeed= 0.1; (* arbitrary for now*)
    position= make_position x y;
  }

let get_position b = 
  b.position

let get_angle b = 
  b.angle 

(** [to_radians x] is degrees [x] to radians *)
let to_radians x = x *. Float.pi /. 180.0

let new_ball_pos_x b = 
  b.position.x +. (b.ballspeed *. cos(to_radians (90.0-.b.angle)))

let new_ball_pos_y b = 
  b.position.y -. (b.ballspeed *. sin(to_radians (90.0-.b.angle)))

let flip_ball_h b = 
  if b.angle > 180.0 then 
    {b with angle = (-1.0) *. b.angle +. 540.0}
  else 
    {b with angle = (-1.0) *. b.angle +. 180.0}


let flip_ball_v b = {b with angle = (-1.0) *. b.angle +. 360.0} 

let step_timer b = { b with timer = b.timer -. 1.0 }

