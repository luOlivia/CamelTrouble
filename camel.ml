open Position

type t = {
  name: string;
  num_bullets: int;
  dir: float; (* degrees 0 - 360 *)
  pos: Position.t
}

let init name x y = {
  name= name;
  num_bullets= 10;
  dir= 0.0;
  pos= make_position x y
}

(** [rot_speed] is speed the tank rotates in degrees *)
let rot_speed = 5.0
(** [fwd_speed] is speed the tank moves forward *)
let fwd_speed = 5.0
(** [rev_speed] is speed the tank moves backward *)
let rev_speed = -0.75

(** [turn_right camel] is [camel] with right rotation *)
let turn_right camel = {camel with dir = Stdlib.mod_float (camel.dir -. rot_speed) 360.0}

(** [turn_left camel] is [camel] with left rotation *)
let turn_left camel = {camel with dir = Stdlib.mod_float (camel.dir +. rot_speed) 360.0}

(** [to_radians x] is degrees [x] to radians *)
let to_radians x = x *. Float.pi /. 180.0

(** [move_horiz x dir] is x pos after moving horizontally *)
let move_horiz x dir speed = 
  x +. (speed *. ((90.0-.dir) |> to_radians |> Stdlib.cos))

(** [move_vert y dir] is y pos after moving vertically *)
let move_vert y dir speed = 
  y -. (speed *. ((90.0-.dir) |> to_radians |> Stdlib.sin))

(** [free_range camel y dir] is camel moved w/o regard to walls *)
let free_range camel speed = 
  let new_x = move_horiz camel.pos.x camel.dir speed in 
  let new_y = move_vert camel.pos.y camel.dir speed in 
  let new_pos = make_position new_x new_y in 
  print_endline (string_of_float new_y);
  {camel with pos = new_pos}

(* 
let corner_collide pos =
  true (* will be abstracted out into main *)

let horiz_collide pos =
  failwith "unimplemented"

let vert_collide pos =
  failwith "unimplemented"

let move camel speed = 
  let new_pos = {
    x= move_horiz camel.pos.x camel.dir speed;
    y= move_vert camel.pos.x camel.dir speed
  } in 
  let after_corner = if corner_collide new_pos 
    then 
      let h_pos = {new_pos with x = move_horiz new_pos.x camel.dir speed} in 
      if corner_collide h_pos || horiz_collide h_pos || vert_collide h_pos
      then 
        let v_pos = {h_pos with y = move_vert new_pos.y camel.dir speed} in 
        if corner_collide v_pos || horiz_collide v_pos || vert_collide v_pos
        then new_pos else v_pos
      else h_pos
    else new_pos in 
  let after_walls = if horiz_collide after_corner && vert_collide after_corner
    then camel.pos (* collision, don't move *)
    else 
    if horiz_collide after_corner then {camel.pos with x = move_horiz camel.pos.x camel.dir speed}
    else if vert_collide after_corner then {camel.pos with y = move_vert camel.pos.y camel.dir speed}
    else after_corner
  in after_walls

let move_fwd camel = {camel with pos = move camel fwd_speed}
let move_rev camel = {camel with pos = move camel rev_speed}
*)
let hit camel = camel
(* let hit camel state = 
   if camel.name = "one" then state.camel1_alive = False else state.camel2_alive = False *)

let to_str camel = 
  camel.name^" num_bullets: "^string_of_int camel.num_bullets^" angle dir: "^string_of_float camel.dir^" pos: "^string_of_float camel.pos.x^" "^string_of_float camel.pos.y