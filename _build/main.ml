open Camel
open State
open Ball
open Maze
open Graphics

let xDimension = 710.0
let yDimension = 710.0
let camel1 = Camel.init "camel1" 120.0 120.0
let camel2 = Camel.init "camel2" 620.0 420.0

let init_state = {
  ball_list= [];
  camel1= camel1;
  camel2= camel2;
  camel1_alive= true;
  camel2_alive= true; 
  maze = make_maze 10
} 

let () = print_endline (init_state.maze |> Maze.to_str)

(** [to_radians x] is degrees [x] to radians *)
let to_radians x = x *. Float.pi /. 180.0

let cosine degree = degree |> to_radians |> Stdlib.cos
let sine degree = degree |> to_radians |> Stdlib.sin

(** [rot_point x y center_x center_y angle] is the 
    point (x,y) rotated around center pt by angle *)
let rot_point x y center_x center_y angle = 
  let x' = x -. center_x in 
  let y' = y -. center_y in 
  let rot_x = x'*.(cosine angle) -. y'*.(sine angle) in
  let rot_y = x'*.(sine angle) +. y'*.(cosine angle) in 
  (rot_x +. center_x |> int_of_float, rot_y +. center_y |> int_of_float)


(** [draw_camel camel color] is the [camel] drawn 
    on maze in direction [dir] *)
let draw_camel camel color =
  set_color color;
  let w = 10.0 in
  let h = 30.0 in
  let d = camel.dir in
  let cx = camel.pos.x in 
  let cy = camel.pos.y in

  let tl_x, tl_y = rot_point (cx-.w/.2.) (cy-.h/.2.) cx cy d in 
  let tr_x, tr_y = rot_point (cx+.w/.2.) (cy-.h/.2.) cx cy d in 
  let br_x, br_y = rot_point (cx+.w/.2.) (cy+.h/.2.) cx cy d in 
  let bl_x, bl_y = rot_point (cx-.w/.2.) (cy+.h/.2.) cx cy d in

  print_endline ("points "^"("^string_of_int tl_x^","^string_of_int tl_y^")");
  fill_poly [|(br_x,br_y);(bl_x,bl_y);(tl_x,tl_y);(tr_x,tr_y)|]



let draw_state state = 
  let halfw = State.wall_width/.2. in 
  let halfh = State.wall_height/.2. in 
  set_color Graphics.red;
  for j = 0 to 6 do
    for i = 0 to 6 do
      (* horizontal *)
      let hlx = (State.square_width +. State.wall_width)*.(i |> float_of_int) in 
      let hly = (State.square_width +. State.wall_width)*.((j |> float_of_int)+.1.) in
      if Maze.is_wall_below init_state.maze i j then 
        fill_rect 
          (hlx |> int_of_float) (hly |> int_of_float) 
          (State.wall_height |> int_of_float) (State.wall_width |> int_of_float);

      (* vertical *)  
      let vlx = (State.square_width +. State.wall_width)*.((i |> float_of_int)+.1.) in 
      let vly = (State.square_width +. State.wall_width)*.(j |> float_of_int) in
      if Maze.is_wall_right state.maze i j  then
        fill_rect 
          (vlx |> int_of_float) (vly |> int_of_float) 
          (State.wall_width |> int_of_float) (State.wall_height |> int_of_float);
    done
  done;

  draw_camel state.camel1 Graphics.black;
  draw_camel state.camel2 Graphics.black

(* let draw_camel camel color = 
   set_color color;
   let curr_x = int_of_float camel.pos.x in 
   let curr_y = int_of_float camel.pos.y in 
   fill_ellipse curr_x curr_y 10 20 *)

(** returns new state with camel moved positions *)
let move_fwd st speed =
  (* let new_camel = move_fwd state camel in  *)
  let new_camel = Camel.free_range st.camel1 speed in 
  print_endline (Camel.to_str new_camel);
  Graphics.clear_graph (); 
  let st' = {st with camel1 = new_camel} in
  draw_state st'; st'

let rotate d st =
  (* let new_camel = move_fwd state camel in  *)
  let new_camel = if d = "left" then Camel.turn_left st.camel1 
    else Camel.turn_right st.camel1 in
  print_endline (Camel.to_str new_camel);
  Graphics.clear_graph (); 
  let st' = {st with camel1 = new_camel} in
  draw_state st'; st'


let input state = 
  match read_key () with
  | 'q' -> print_endline "exit"; exit 0
  | 'w' -> print_endline "player 1 up"; move_fwd state (-5.0)
  | 'a' -> print_endline "player 1 left"; rotate "left" state
  | 's' -> print_endline "player 1 down"; move_fwd state (5.0)
  | 'd' -> print_endline "player 1 right"; rotate "right" state
  | _ -> print_endline "sir pls"; state

(** [run] displays the game window and allows users to quit with key q *)
let rec run state =
  let new_state = input state in
  run new_state

let floaty i = i |> float_of_int

let init = Graphics.open_graph "";
  set_window_title "Camel Trouble";
  resize_window (xDimension |> int_of_float) (int_of_float yDimension);
  draw_state init_state;

  run init_state

