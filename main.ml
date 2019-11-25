open Camel
open State
open Ball
open Maze
open Graphics
open Unix 

(* let fake_maze = make_maze 20
   let fake_maze = make_maze 20
   let fake_maze = make_maze 20
   let fake_maze = make_maze 20 *)

let () = print_endline (State.init_state.maze |> Maze.to_str)

(** [draw_camel camel color] is the [camel] drawn 
    on maze in direction [dir] *)
let draw_camel camel color =
  set_color color;
  let w = 10.0 in
  let h = camel_width in
  let d = camel.dir in
  let cx = camel.pos.x in 
  let cy = camel.pos.y in

  let tl_x, tl_y = State.rot_point (cx-.w/.2.) (cy-.h/.2.) cx cy d in 
  let tr_x, tr_y = State.rot_point (cx+.w/.2.) (cy-.h/.2.) cx cy d in 
  let br_x, br_y = State.rot_point (cx+.w/.2.) (cy+.h/.2.) cx cy d in 
  let bl_x, bl_y = State.rot_point (cx-.w/.2.) (cy+.h/.2.) cx cy d in

  (* print_endline ("points "^"("^string_of_int tl_x^","^string_of_int tl_y^")"); *)
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
      if Maze.is_wall_below state.maze i j then 
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

(*     
let move_rev st =
  let new_camel = move_rev st st.camel1 in 
  print_endline (Camel.to_str new_camel);
  Graphics.clear_graph (); 
  let st' = {st with camel1 = new_camel} in
  draw_state st'; st' *)

(** [flush_kp () flushes keypress queue *)
let flush_kp () = while key_pressed () do
    let c = read_key ()
    in ()
  done

let input state = 
  if not (Graphics.key_pressed ()) then State.update_state state
  else let k = Graphics.read_key () in
    flush_kp ();
    let state' = State.update_state state in
    print_endline (Char.escaped k);
    match k with
    | '0' -> exit 0
    | 'w' -> State.move `Forward state'
    | 'a' -> State.rotate `Left state'
    | 's' -> State.move `Reverse state'
    | 'd' -> State.rotate `Right state'
    | 'e' -> State.shoot state'.camel1 state'
    | _ -> state'

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
  resize_window (State.xDimension) (State.yDimension);
  draw_state State.init_state;
  run State.init_state 




























































