open Camel
open State
open Ball
open Maze
open Graphics
open Unix 

let xDimension = 8.0 *. State.wall_width +. 7.0 *. State.square_width |> int_of_float
let yDimension = xDimension
let camel1 = Camel.init "camel1" 120.0 120.0
let camel2 = Camel.init "camel2" 620.0 420.0

let init_state = {
  ball_list= [];
  camel1= camel1;
  camel2= camel2;
  camel1_alive= true;
  camel2_alive= true; 
  maze = make_maze 20
} 

let () = print_endline (init_state.maze |> Maze.to_str)

(** [to_radians x] is degrees [x] to radians *)
let to_radians x = x *. Float.pi /. 180.0

(** [cosine degree] is cosine of degrees *)
let cosine degree = degree |> to_radians |> Stdlib.cos
(** [sine degree] is sine of degrees *)
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

let draw_balls state color = 
  set_color color;

  let rec iter_balls = function
    | [] -> () 
    | ball::t -> fill_circle (ball.position.x |> int_of_float) (ball.position.y |> int_of_float) 3; iter_balls t 
  in iter_balls state.ball_list 

let draw_state state = 
  (* set background color to sand *)
  (* set_color (Graphics.rgb 255 251 224);
     fill_rect 0 0 xDimension yDimension; *)

  (* draws walls *)
  let halfw = State.wall_width/.2. in 
  let halfh = State.wall_height/.2. in 
  set_color (Graphics.rgb 4 145 23);

  for i = 0 to 6 do
    for j = 0 to 6 do
      (* horizontal walls *)
      let hlx = (State.square_width +. State.wall_width)*.(i |> float_of_int) +. State.wall_width in 
      let hly = (State.square_width +. State.wall_width)*.((j |> float_of_int)+.1.)  in
      if Maze.is_wall_below init_state.maze i j then 
        fill_rect 
          (hlx |> int_of_float) (hly |> int_of_float) 
          (State.wall_height |> int_of_float) (State.wall_width |> int_of_float);

      if j = 0 then 
        fill_rect 
          (hlx |> int_of_float) (hly -. State.square_width -. State.wall_width |> int_of_float) 
          (State.wall_height |> int_of_float) (State.wall_width |> int_of_float);

      (* vertical walls *)  
      let vlx = (State.square_width +. State.wall_width)*.((i |> float_of_int)+.1.) in 
      let vly = (State.square_width +. State.wall_width)*.(j |> float_of_int)+. State.wall_width in

      if Maze.is_wall_right state.maze i j then
        fill_rect 
          (vlx |> int_of_float) (vly |> int_of_float) 
          (State.wall_width |> int_of_float) (State.wall_height |> int_of_float);

      if i = 0 then 
        fill_rect 
          (vlx -. State.square_width -. State.wall_width |> int_of_float) (vly  |> int_of_float) 
          (State.wall_width |> int_of_float) (State.wall_height |> int_of_float);

    done
  done;

  (* draws camel players *)
  draw_camel state.camel1 Graphics.black;
  draw_camel state.camel2 Graphics.black;

  (* draw balls *)
  draw_balls state Graphics.black

(**[move_all_balls player st] is the new state after all balls have been moved.*)
let move_all_balls st = 
  (* let rec iter_balls blst = function
     | [] -> blst 
     | ball::t -> iter_balls ((State.move_ball st ball)::blst) t
     in let blst' = iter_balls [] (st.ball_list) in  *)
  let blst' = List.fold_left (fun a x -> (State.move_ball st x)::a) [] st.ball_list in 
  {st with ball_list = blst'}

let rec update_state state = 
  move_all_balls state 

(** returns new state with camel moved positions *)
let move direction st =
  let new_camel = match direction with 
    | `Forward -> move_fwd st st.camel1
    | `Reverse -> move_rev st st.camel1 in
  print_endline (Camel.to_str new_camel);
  let st' = {st with camel1 = new_camel} in st'

(*     
let move_rev st =
  let new_camel = move_rev st st.camel1 in 
  print_endline (Camel.to_str new_camel);
  Graphics.clear_graph (); 
  let st' = {st with camel1 = new_camel} in
  draw_state st'; st' *)

let rotate d st =
  (* let new_camel = move_fwd state camel in  *)
  let new_camel = match d with
    | `Left -> Camel.turn_left st.camel1
    | `Right -> Camel.turn_right st.camel1 in
  print_endline (Camel.to_str new_camel);
  let st' = {st with camel1 = new_camel} in st'

let shoot player st = 
  let xpos, ypos = player.pos.x, player.pos.y in 
  let newball = Ball.init player player.dir xpos ypos in 
  let st' = {st with ball_list=(newball::st.ball_list)} in st'

(** [flush_kp () flushes keypress queue *)
let flush_kp () = while key_pressed () do
    let c = read_key ()
    in ()
  done

let input state = 
  if not (Graphics.key_pressed ()) then update_state state
  else let k = Graphics.read_key () in
    flush_kp ();
    match k with
    | '0' -> print_endline "exit"; exit 0
    | 'w' -> print_endline "player 1 up"; move `Forward state
    | 'a' -> print_endline "player 1 left"; rotate `Left state
    | 's' -> print_endline "player 1 down"; move `Reverse state 
    | 'd' -> print_endline "player 1 right"; rotate `Right state
    | 'e' -> print_endline "player 1 shooting"; shoot state.camel1 state
    | _ -> print_endline "sir pls"; state

(** [run] displays the game window and allows users to quit with key 0
    refactor later bc alex *)
let rec run state =
  let new_state = input state in
  Graphics.auto_synchronize false;
  Graphics.clear_graph (); 
  draw_state new_state;
  Graphics.auto_synchronize true;
  run new_state

let init = Graphics.open_graph "";
  set_window_title "Camel Trouble";
  resize_window (xDimension) (yDimension);
  draw_state init_state;
  run init_state 


























































