open Camel
open State
open Ball
open Maze
open Graphics_js
open Unix 

(** [keys] is a record of whether a set of keys is pressed [true] 
    or not pressed [false] *)
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

(** [input_keys] is the default state of the game keys, with none pressed *)
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

(**[commands] represents a player keyboard command*)
type commands = Player of controls

(**[controls] represents player keyboard commands *)
and controls = 
  | Shoot
  | Left 
  | Right
  | Up 
  | Down

(** [current_state] repsame state *)
let current_state = ref State.init_state

(** [draw_camel camel color] draws the [camel] 
    on the maze in direction [dir] *)
let draw_camel camel color hump =
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

  Graphics_js.set_color Graphics.black;
  Graphics_js.set_color color;
  Graphics_js.fill_poly [|(br_x,br_y);(bl_x,bl_y);(tl_x,tl_y);(tr_x,tr_y)|];
  Graphics_js.fill_ellipse (tl_x + (tr_x-tl_x)/2) (tl_y + (tr_y-tl_y)/2) 2 3;
  Graphics_js.fill_ellipse (bl_x + (br_x-bl_x)/2) (bl_y + (br_y-bl_y)/2) 2 5;
  Graphics_js.set_color hump;
  Graphics_js.fill_circle (int_of_float cx) (int_of_float cy) 4;
  Graphics_js.fill_circle (tl_x-1) (tl_y-1) 3;
  Graphics_js.fill_circle (tr_x-1) (tr_y-1) 3

(** [draw_balls] draws all active balls in [state] onto the canvas.*)
let draw_balls state color = 
  Graphics_js.set_color color;
  let f ball = 
    Graphics_js.fill_circle 
      (ball.position.x |> int_of_float) 
      (ball.position.y |> int_of_float) 
      (State.ball_width /. 2.0 |> int_of_float) in
  List.iter f state.ball_list

(** [draw_state] draws the current [state] of the game *)
let draw_state state = 
  Resources.draw "player1" 450 350;
  Resources.draw "player2" 600 350;

  (* set background color to sand *)
  Graphics_js.set_color Resources.sand;
  Graphics_js.fill_rect 0 0 xDimension yDimension;

  (* prints score keeping *)
  Resources.draw_string Graphics.black 20 (xDimension+10) 30 
    ("Player 1 score: " ^ (state.camel1.score |> string_of_int));
  Resources.draw_string Graphics.black 20 (xDimension+10) 10 
    ("Player 2 score: " ^ (state.camel2.score |> string_of_int));

  (* draws walls *)
  Graphics_js.set_color Resources.wall_color;

  for i = 0 to Maze.num_grid_squares - 1 do
    for j = 0 to Maze.num_grid_squares - 1 do
      (* horizontal walls *)
      let hlx = (State.square_width +. State.wall_width)*.
                (i |> float_of_int) +. State.wall_width in 
      let hly = (State.square_width +. State.wall_width)*.
                (j + 1 |> float_of_int) in
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
                ((i + 1 |> float_of_int)) in 
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
  draw_camel state.camel1 Resources.camel_c1 Resources.camel_c1_hump;
  draw_camel state.camel2 Resources.camel_c2 Resources.camel_c2_hump;

  (* draw balls *)
  draw_balls state Graphics_js.black

(**[keypressed] is an eventlistener that updates the state of the [input_keys]
   in the [evt] that any key is pressed. *)
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

(**[keyup] is an eventlistener that updates the state of the [input_keys] in
   the [evt] that any key is unpressed.*)
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

(* let translate_keys () =
   let k = input_keys in
   let ctrls = [(k.left,CLeft);(k.right,CRight);(k.up,CUp);(k.down,CDown)] in
   List.fold_left (fun a x -> if fst x then (snd x)::a else a) [] ctrls *)

(**[input state] is the new [state] based on changes to *)
let input state = 
  let state' = State.update_state state in
  let state'' = 
    if input_keys.p1_shoot then State.shoot state'.camel1 state'
    else if input_keys.p1_left then State.rotate CounterClockwise state' state'.camel1  
    else if input_keys.p1_right then State.rotate Clockwise state' state'.camel1
    else if input_keys.p1_down then State.move Reverse state' state'.camel1
    else if input_keys.p1_up then State.move Forward state' state'.camel1 
    else state' in 
  if input_keys.p2_shoot then State.shoot state''.camel2 state''  
  else if input_keys.p2_left then State.rotate CounterClockwise state'' state''.camel2  
  else if input_keys.p2_right then State.rotate Clockwise state'' state''.camel2
  else if input_keys.p2_down then State.move Reverse state'' state''.camel2
  else if input_keys.p2_up then State.move Forward state'' state''.camel2 
  else state''

let end_game winner = 
  Graphics_js.set_color Resources.sand;
  Graphics_js.fill_rect 0 0 xDimension yDimension;
  Graphics.set_text_size 30;
  (* Graphics.draw_string (winner^" WON!"); *)
  let c, player, dark = match winner with
    | One -> Resources.blue_grad, "PLAYER 1", Resources.blue1
    | Two -> Resources.red_grad, "PLAYER 2", Resources.red1 in 
  ignore (Resources.gradient_text c 80 240 
            (player^" WON!") (Resources.size+5));
  ignore (Resources.draw_string dark 20 80 120 "press enter to replay");
  Graphics_js.set_text_size 10;
  Resources.draw "desert" 10 80

(* TODO: REFACTOR!!! *)
let calc_fps t0 t1 =
  let delta = (t1 -. t0) /. 1000. in
  1. /. delta
let last_time = ref 0.

(** [run] displays the game window and allows users to quit with key 0*)
let rec run time state =
  let fps = calc_fps !last_time time in
  last_time := time;

  let new_state = input state in
  Graphics_js.clear_graph (); 
  if not state.camel1_alive then 
    begin current_state := {new_state with camel1_alive = true; ball_list = []; status = Paused}; end_game Two end
  else if not state.camel2_alive then 
    begin current_state := {new_state with camel2_alive = true; ball_list = []; status = Paused}; end_game One end
  else (draw_state new_state;
        Js_of_ocaml.Dom_html.window##requestAnimationFrame(
          Js_of_ocaml.Js.wrap_callback (fun (t:float) -> run time new_state )
        ) |> ignore ) 

(* let tate = ref (Lwt.task ()) in fst !tate  *)


(* let init = Graphics_js.open_graph "";
   set_window_title "Camel Trouble";
   resize_window (State.xDimension) (State.yDimension);
   draw_state State.init_state;
   run State.init_state  *)


let init () = 
  print_endline "we in init now bois and girls";
  draw_state State.init_state; run 0.0 State.init_state


let rec main () =
  (* sets intial game start page *)
  Graphics_js.set_color Resources.sand;
  Graphics_js.fill_rect 0 0 xDimension yDimension;
  ignore (Resources.gradient_text Resources.purple_grad 60 225 
            "PRESS ENTER TO PLAY" (Resources.size+5));
  Graphics_js.set_text_size 10;
  Resources.draw "desert" 10 80

let press_start evt =
  print_endline "in press start";
  match !current_state.status with 
  | Start | Paused -> begin
      let () = match evt##.keyCode with
        | 13  -> begin
            current_state := {!current_state with status = Playing};  
            draw_state !current_state; run 0.0 !current_state (*init ()*)
          end
        | _ -> ()
      in Js_of_ocaml.Js._true
    end 
  | _ ->  Js_of_ocaml.Js._true

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