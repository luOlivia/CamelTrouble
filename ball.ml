open Position
open Utils

(* 
- timer 
- owner 
- angle 
- position (type point)
- ballspeed 
- engine 
*)

let ball_timer = 10.0
let ball_speed = 5.0
let timer_decrement = 0.1

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
    timer= ball_timer;
    owner= owner;
    angle= a;
    ballspeed= ball_speed;
    position= make_position x y;
  }

let get_position b = 
  b.position

let get_angle b = 
  b.angle 

let new_ball_pos_x b = 
  b.position.x +. (b.ballspeed *. cosine (90.0 -. b.angle))

let new_ball_pos_y b = 
  b.position.y -. (b.ballspeed *. sine (90.0 -. b.angle))

let flip_ball_h b = 
  if b.angle > 180.0 then 
    {b with angle = (-1.0) *. b.angle +. 540.0}
  else 
    {b with angle = (-1.0) *. b.angle +. 180.0}

let flip_ball_v b = 
  {b with angle = (-1.0) *. b.angle +. 360.0} 

let step_timer b = 
  {b with timer = b.timer -. timer_decrement}

