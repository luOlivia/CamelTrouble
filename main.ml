open Camel
open State
open Ball
open Maze
open Graphics_js
open Unix 

(* let fake_maze = make_maze 20
   let fake_maze = make_maze 20
   let fake_maze = make_maze 20
   let fake_maze = make_maze 20 *)
(* let () = print_endline (State.init_state.maze |> Maze.to_str) *)


type keys = {
  mutable p1_up : bool;
  mutable p1_left : bool;
  mutable p1_right : bool;
  mutable p1_down : bool;
  mutable p1_shoot : bool;
  mutable p2_up : bool;
  mutable p2_left : bool;
  mutable p2_right : bool;
  mutable p2_down : bool;
  mutable p2_shoot : bool;
}

let input_keys = {
  p1_up = false;
  p1_left = false;
  p1_right = false;
  p1_down = false;
  p1_shoot = false;
  p2_up = false;
  p2_left = false;
  p2_right = false;
  p2_down = false;
  p2_shoot = false;
}


let my_state = ref State.init_state



(** [draw_camel camel color] is the [camel] drawn 
    on maze in direction [dir] *)
let draw_camel camel color =
  Graphics_js.set_color color;
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
  Graphics_js.fill_poly [|(br_x,br_y);(bl_x,bl_y);(tl_x,tl_y);(tr_x,tr_y)|]
(* Graphics_js.fill_rect (int_of_float cx) (int_of_float cy) (intbitch_of_float w) (int_of_float h) *)

let draw_balls state color = 
  Graphics_js.set_color color;
  let rec iter_balls = function
    | [] -> () 
    | ball::t -> Graphics_js.fill_circle 
                   (ball.position.x |> int_of_float) 
                   (ball.position.y |> int_of_float) 3;
      iter_balls t 
  in iter_balls state.ball_list 

let draw_state state = 
  Resources.draw "player1" 400 350;
  Resources.draw "player2" 550 350;


  (* set background color to sand *)
  Graphics_js.set_color (Graphics_js.rgb 252 213 145);
  Graphics_js.fill_rect 0 0 xDimension yDimension;
  Graphics_js.set_color Graphics.black;
  Graphics.moveto (xDimension+10) 10; 
  Graphics.draw_string ("Player 1 score: "^(string_of_int state.camel1.score));
  Graphics.moveto (xDimension+10) 30; 
  Graphics.draw_string ("Player 2 score: "^(string_of_int state.camel2.score));
  Graphics_js.set_color Resources.sand;
  Graphics_js.fill_rect 0 0 xDimension yDimension;

  (* draws walls *)

  let halfw = State.wall_width/.2. in 
  let halfh = State.wall_height/.2. in 
  Graphics_js.set_color Resources.wall_color;

  for i = 0 to Maze.num_grid_squares - 1 do
    for j = 0 to Maze.num_grid_squares - 1 do
      (* horizontal walls *)
      let hlx = (State.square_width +. State.wall_width)*.
                (i |> float_of_int) +. State.wall_width in 
      let hly = (State.square_width +. State.wall_width)*.
                ((j |> float_of_int)+.1.)  in
      if Maze.is_wall_below state.maze i j then 
        Graphics_js.fill_rect 
          (hlx |> int_of_float) (hly |> int_of_float) 
          (State.wall_height |> int_of_float) 
          (State.wall_width |> int_of_float);

      if j = 0 then 
        Graphics_js.fill_rect 
          (hlx |> int_of_float) 
          (hly -. State.square_width -. State.wall_width |> int_of_float) 
          (State.wall_height |> int_of_float) 
          (State.wall_width |> int_of_float);

      (* vertical walls *)  
      let vlx = (State.square_width +. State.wall_width)*.
                ((i |> float_of_int)+.1.) in 
      let vly = (State.square_width +. State.wall_width)*.
                (j |> float_of_int)+. State.wall_width in

      if Maze.is_wall_right state.maze i j then
        Graphics_js.fill_rect 
          (vlx |> int_of_float) (vly |> int_of_float) 
          (State.wall_width |> int_of_float) 
          (State.wall_height |> int_of_float);

      if i = 0 then 
        Graphics_js.fill_rect 
          (vlx -. State.square_width -. State.wall_width |> int_of_float) 
          (vly  |> int_of_float) 
          (State.wall_width |> int_of_float) 
          (State.wall_height |> int_of_float);

    done
  done;


  (* draws camel players *)
  draw_camel state.camel1 Graphics_js.blue;
  draw_camel state.camel2 Graphics_js.red;

  (* draw balls *)
  draw_balls state Graphics_js.black


let keypressed evt =
  let () = match evt##.keyCode with
    | 38  -> input_keys.p2_up<- true
    | 39 -> input_keys.p2_right <- true
    | 37 -> input_keys.p2_left <- true
    | 40 -> input_keys.p2_down <- true
    | 190 -> input_keys.p2_shoot <- true
    | 87 -> input_keys.p1_up<- true
    | 68 -> input_keys.p1_right <- true
    | 65 -> input_keys.p1_left <- true
    | 83 -> input_keys.p1_down <- true
    | 69 -> input_keys.p1_shoot <- true
    | _ -> ()
  in Js_of_ocaml.Js._true

(* Keyup event handler translates a key release *)
let keyup evt =
  let () = match evt##.keyCode with
    | 38  -> input_keys.p2_up<- false
    | 39 -> input_keys.p2_right <- false
    | 37 -> input_keys.p2_left <- false
    | 40 -> input_keys.p2_down <- false
    | 190 -> input_keys.p2_shoot <- false
    | 87 -> input_keys.p1_up<- false
    | 68 -> input_keys.p1_right <- false
    | 65 -> input_keys.p1_left <- false
    | 83 -> input_keys.p1_down <- false
    | 69 -> input_keys.p1_shoot <- false
    | _ -> ()
  in Js_of_ocaml.Js._true


(** [flush_kp () flushes keypress queue *)
let flush_kp () = while key_pressed () do
    let c = read_key ()
    in ()
  done



let input state = 
  let state' = State.update_state state in
  let state'' = 
    if input_keys.p1_shoot then  State.shoot state'.camel1 state'
    else if input_keys.p1_left then State.rotate `Left state' state'.camel1  
    else if input_keys.p1_right then State.rotate `Right state' state'.camel1
    else if input_keys.p1_down then State.move `Reverse state' state'.camel1
    else if input_keys.p1_up then State.move `Forward state' state'.camel1 
    else state' in 
  if input_keys.p2_shoot then State.shoot state''.camel2 state''  
  else if input_keys.p2_left then State.rotate `Left state'' state''.camel2  
  else if input_keys.p2_right then State.rotate `Right state'' state''.camel2
  else if input_keys.p2_down then State.move `Reverse state'' state''.camel2
  else if input_keys.p2_up then State.move `Forward state'' state''.camel2 
  else state''

(* TODO: REFACTOR!!! *)
let clear_canvas canvas =
  let context = canvas##getContext (Js_of_ocaml.Dom_html._2d_) in
  let cwidth = float_of_int canvas##width in
  let cheight = float_of_int canvas##height in
  ignore (context##clearRect(0.,0.,cwidth,cheight))

(* TODO: REFACTOR!!! *)
let calc_fps t0 t1 =
  let delta = (t1 -. t0) /. 1000. in
  1. /. delta
let last_time = ref 0.

let end_game winner = 
  Graphics_js.set_color Resources.sand;
  Graphics_js.fill_rect 0 0 xDimension yDimension;
  Graphics.set_text_size 30;
  (* Graphics.draw_string (winner^" WON!"); *)
  let c = if winner = "PLAYER 1" then Resources.blue_grad else Resources.red_grad in 
  ignore (Resources.gradient_text c 80 240 
            (winner^" WON!") (Resources.size+5));
  let dark = if winner = "PLAYER 1" then Resources.blue1 else Resources.red1 in
  ignore (Resources.draw_string dark 20 80 120 "press enter to replay");
  Graphics_js.set_text_size 10;
  Resources.draw "desert" 10 80


(** [run] displays the game window and allows users to quit with key 0
    refactor later bc alex *)


let rec run time state =
  let fps = calc_fps !last_time time in
  last_time := time;

  let new_state = input state in
  Graphics_js.clear_graph (); 
  if state.camel1_alive = false then 
    begin my_state := {new_state with camel1_alive = true}; end_game "PLAYER 2" end
  else if state.camel2_alive = false then 
    begin my_state := {new_state with camel2_alive = true}; end_game "PLAYER 1" end
  else draw_state new_state;

  Js_of_ocaml.Dom_html.window##requestAnimationFrame(
    Js_of_ocaml.Js.wrap_callback (fun (t:float) -> run time new_state )
  ) |> ignore 

(* let tate = ref (Lwt.task ()) in fst !tate  *)


(* let init = Graphics_js.open_graph "";
   set_window_title "Camel Trouble";
   resize_window (State.xDimension) (State.yDimension);
   draw_state State.init_state;
   run State.init_state  *)

(* 
let init () = 
  print_endline "we in init now bois and girls";
  draw_state State.init_state; run 0.0 State.init_state *)


let rec main () =
  (* sets intial game start page *)
  Graphics_js.set_color Resources.sand;
  Graphics_js.fill_rect 0 0 xDimension yDimension;
  ignore (Resources.gradient_text Resources.purple_grad 55 225 
            "PRESS ENTER TO PLAY" (Resources.size+5));
  Graphics_js.set_text_size 10;
  Resources.draw "desert" 10 80

let press_start evt =
  print_endline "in press start";
  let () = match evt##.keyCode with
    | 13  -> draw_state !my_state; run 0.0 !my_state (*init ()*)
    | _ -> ()
  in Js_of_ocaml.Js._true

(* refactoring keydown listeners *)
let _ = 
  List.iter (fun f -> begin 
        ignore(Js_of_ocaml.Dom_html.addEventListener 
                 Js_of_ocaml.Dom_html.document 
                 Js_of_ocaml.Dom_html.Event.keydown 
                 (Js_of_ocaml.Dom_html.handler f) Js_of_ocaml.Js._true )
      end) [press_start; keypressed]

let _ =  print_endline "starting up";
  Js_of_ocaml.Js.Opt.iter
    (Js_of_ocaml.Dom_html.CoerceTo.canvas (Js_of_ocaml.Dom_html.getElementById "canvas2"))
    Graphics_js.open_canvas

(* let _ = Js_of_ocaml.Dom_html.addEventListener Js_of_ocaml.Dom_html.document 
    Js_of_ocaml.Dom_html.Event.keydown (Js_of_ocaml.Dom_html.handler press_start) Js_of_ocaml.Js._true 
   let _ = Js_of_ocaml.Dom_html.addEventListener Js_of_ocaml.Dom_html.document 
    Js_of_ocaml.Dom_html.Event.keydown (Js_of_ocaml.Dom_html.handler keypressed) Js_of_ocaml.Js._true  *)
let _ = Js_of_ocaml.Dom_html.addEventListener Js_of_ocaml.Dom_html.document 
    Js_of_ocaml.Dom_html.Event.keyup (Js_of_ocaml.Dom_html.handler keyup) Js_of_ocaml.Js._true 


let () = main ()
