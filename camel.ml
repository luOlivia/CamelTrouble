open Position
open Utils

type player_num = One | Two

type t = {
  player_num: player_num;
  player_name: string;
  score: int;
  num_balls: int;
  dir: float; (* degrees 0 - 360 *)
  pos: Position.t;
  shot_time: float; 
}

let init num x y score name = {
  player_num = num;
  player_name = name;
  score = score;
  num_balls = 0;
  dir = Random.float 359.0;
  pos = Position.init x y;
  shot_time = 0.0; 
}

(** [rot_speed] is speed the tank rotates in degrees *)
let rot_speed = 5.0
(** *)
let fwd_speed = 5.0
let rev_speed = -3.0

let turn_right camel = 
  {camel with dir = Stdlib.mod_float (camel.dir -. rot_speed) 360.0}

let turn_left camel = 
  {camel with dir = Stdlib.mod_float (camel.dir +. rot_speed) 360.0}

let move_horiz x dir speed = 
  x +. (speed *. cosine (90.0-.dir))

let move_vert y dir speed = 
  y -. (speed *. sine (90.0-.dir))

let free_range camel speed = 
  let new_x = move_horiz camel.pos.x camel.dir speed in 
  let new_y = move_vert camel.pos.y camel.dir speed in 
  let new_pos = Position.init new_x new_y in 
  {camel with pos = new_pos}

let to_str camel = 
  let player = match camel.player_num with
    | One -> "player one"
    | Two -> "player two" in
  player 
  ^ " num_balls: " ^ string_of_int camel.num_balls ^ "\n"
  ^ " angle dir: " ^ string_of_float camel.dir ^ "\n"
  ^ " pos: " ^ string_of_float camel.pos.x 
  ^ " " ^ string_of_float camel.pos.y