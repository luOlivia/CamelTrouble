open Position
open Utils

type t = 
  {
    player_num: int;
    num_bullets: int;
    dir: float; (* degrees 0 - 360 *)
    pos: Position.t;
    shot_time: float; 
  }

let init player_num x y =
  {
    player_num=player_num;
    num_bullets= 0;
    dir= 0.0;
    pos= make_position x y;
    shot_time = 0.0; 
  }

(** [rot_speed] is speed the tank rotates in degrees *)
let rot_speed = 5.0
let fwd_speed = 5.0
let rev_speed = -3.0

let turn_right camel = 
  {camel with dir = Stdlib.mod_float (camel.dir -. rot_speed) 360.0}

let turn_left camel = 
  {camel with dir = Stdlib.mod_float (camel.dir +. rot_speed) 360.0}

(** [to_radians x] is degrees [x] to radians *)
let to_radians x = x *. Float.pi /. 180.0

let move_horiz x dir speed = 
  x +. (speed *. cosine (90.0-.dir))

let move_vert y dir speed = 
  y -. (speed *. sine (90.0-.dir))

let free_range camel speed = 
  let new_x = move_horiz camel.pos.x camel.dir speed in 
  let new_y = move_vert camel.pos.y camel.dir speed in 
  let new_pos = make_position new_x new_y in 
  {camel with pos = new_pos}

(* let hit camel = false *)
(* failwith "Unimplemented" *)

let to_str camel = 
  string_of_int camel.player_num^" num_bullets: "^string_of_int camel.num_bullets^
  " angle dir: "^string_of_float camel.dir^" pos: "^
  string_of_float camel.pos.x^" "^string_of_float camel.pos.y