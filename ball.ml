open Position
open Utils

(* [ball_timer] is a constant controlling the duration of a ball *)
let ball_timer = 20.0
(* [ball_speed] is a constant controlling the speed of a ball *)
let ball_speed = 5.0
(* [timer_decrement] is a constant controlling the rate of exprity of a ball *)
let timer_decrement = 0.1

type t = {
  timer: float;
  owner: Camel.t;
  angle: float;
  position: Position.t;
  speed: float;
}

let init owner angle x y = {
  timer = ball_timer;
  owner = owner;
  angle = angle;
  speed = ball_speed;
  position = Position.init x y;
}

let new_ball_pos_x b = 
  b.position.x +. (b.speed *. cosine (90.0 -. b.angle))

let new_ball_pos_y b = 
  b.position.y -. (b.speed *. sine (90.0 -. b.angle))

let flip_ball_h b = 
  let angle' = if b.angle > 180.0 then 
      (-1.0) *. b.angle +. 540.0
    else 
      (-1.0) *. b.angle +. 180.0
  in {b with angle=angle'}

let flip_ball_v b = 
  {b with angle = (-1.0) *. b.angle +. 360.0} 

let step_timer b = 
  {b with timer = b.timer -. timer_decrement}

