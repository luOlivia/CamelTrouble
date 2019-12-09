open Camel
open State
open Ball
open Maze
open Graphics_js
open Unix 
open Utils

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

(**[control] represents player keyboard commands *)
type control = 
  | Shoot
  | Left 
  | Right
  | Up 
  | Down

(**[command] represents a player keyboard command*)
type command = Camel.player_num * control

let map_keys () =
  let k = input_keys in
  let cmds = 
    [
      (k.p1_up,(One, Up));
      (k.p1_down,(One, Down));
      (k.p1_left,(One, Left));
      (k.p1_right,(One, Right));
      (k.p1_shoot,(One, Shoot));
      (k.p2_up,(Two, Up));
      (k.p2_down,(Two, Down));
      (k.p2_left,(Two, Left));
      (k.p2_right,(Two, Right));
      (k.p2_shoot,(Two, Shoot))
    ] in
  let f (a1, a2) x =
    match fst x with 
    | false -> (a1, a2)
    | true -> begin
        match snd x with 
        | One, ctrl -> ctrl::a1, a2
        | Two, ctrl -> a1, ctrl::a2
      end in
  List.fold_left f ([], []) (cmds)

(** [current_state] repsame state *)
let current_state = ref State.init_state

let get_fps t0 t1 =
  1.0 /. ((t1 -. t0) /. 1000.0)
let prev_time = ref 0.0
let fps = ref 0.0

let draw_players () = 
  Resources.draw "player1" 450 350;
  Resources.draw "player2" 600 350

let draw_background () =
  Graphics_js.set_color Resources.sand;
  Graphics_js.fill_rect 0 0 xDimension yDimension

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
  Graphics_js.fill_circle (tl_x-1) (tl_y-1) 2;
  Graphics_js.fill_circle (tr_x-1) (tr_y-1) 2;
  Resources.draw_string Graphics.black 10 ((int_of_float cx)-20) 
    ((int_of_float cy) + 15) camel.player_name


let draw_scores state = 
  Resources.draw_string Graphics.black 20 (xDimension+10) 30 
    (state.camel1.player_name^"'s score: " ^ 
     (state.camel1.score |> string_of_int));
  Resources.draw_string Graphics.black 20 (xDimension+10) 10 
    (state.camel2.player_name^"'s score: " ^ 
     (state.camel2.score |> string_of_int))

let draw_walls state = 
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
  done

let draw_all_camels state =
  draw_camel state.camel1 Resources.camel_c1 Resources.camel_c1_hump;
  draw_camel state.camel2 Resources.camel_c2 Resources.camel_c2_hump

(** [draw_balls] draws all active balls in [state] onto the canvas.*)
let draw_balls state = 
  Graphics_js.set_color Graphics_js.black;
  let f ball = 
    Graphics_js.fill_circle 
      (ball.position.x |> int_of_float) 
      (ball.position.y |> int_of_float) 
      (State.ball_width /. 2.0 |> int_of_float) in
  List.iter f state.ball_list

let draw_fps () =
  Resources.draw_string Graphics.black 10 (xDimension+10) (xDimension-10) 
    ((!fps |> truncate |> string_of_float)^" frames per second") 

(** [draw_state] draws the current [state] of the game *)
let draw_state state = 
  draw_players ();
  draw_background ();
  draw_scores state;
  draw_walls state;
  draw_all_camels state;
  draw_balls state;
  draw_fps ()

(**[keypressed] is an eventlistener that updates the state of the [input_keys]
   in the [evt] that any key is pressed. *)
let keypressed evt =
  let () = match evt##.keyCode with
    | 38  -> input_keys.p2_up <- true
    | 39 -> input_keys.p2_right <- true
    | 37 -> input_keys.p2_left <- true
    | 40 -> input_keys.p2_down <- true
    | 190 -> input_keys.p2_shoot <- true
    | 87 -> input_keys.p1_up <- true
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
    | 38  -> input_keys.p2_up <- false
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

let handle_ctrls player ctrls st = 
  let camel = match player with
    | One -> st.camel1
    | Two -> st.camel2 in
  match List.nth_opt ctrls 0 with 
  | None -> st
  | Some ctrl ->
    begin match ctrl with 
      | Shoot -> State.shoot camel st
      | Left -> State.rotate CounterClockwise st camel
      | Right -> State.rotate Clockwise st camel
      | Down -> State.move Reverse st camel
      | Up -> State.move Forward st camel
    end

let input st = 
  let player_1_ctrls, player_2_ctrls = map_keys () in
  st 
  |> State.update_state
  |> handle_ctrls One player_1_ctrls
  |> handle_ctrls Two player_2_ctrls

let end_game st winner = 
  Graphics_js.set_color Resources.sand;
  Graphics_js.fill_rect 0 0 xDimension yDimension;
  Graphics.set_text_size 30;
  (* Graphics.draw_string (winner^" WON!"); *)
  let c, player, dark = match winner with
    | One -> Resources.blue_grad, st.camel1.player_name, Resources.blue1
    | Two -> Resources.red_grad, st.camel2.player_name, Resources.red1 in 
  ignore (Resources.gradient_text c 80 240 
            (player^" WON!") (Resources.size+5));
  ignore (Resources.draw_string dark 20 80 120 "press enter to replay");
  Graphics_js.set_text_size 10;
  Resources.draw "desert" 10 80

let handle_death_screen st player = 
  let st' = {st with ball_list=[]; status=Paused} in
  match player with 
  | One -> current_state := {st' with camel1_alive=true}; end_game st Two
  | Two -> current_state := {st' with camel2_alive=true}; end_game st One

(** [run] displays the game window and allows users to quit with key 0*)
let rec run time state =
  fps := get_fps !prev_time time;
  prev_time := time;

  let new_state = input state in
  Graphics_js.clear_graph (); 
  if not state.camel1_alive 
  then handle_death_screen new_state One
  else if not state.camel2_alive 
  then handle_death_screen new_state Two
  else begin 
    draw_state new_state;
    Js_of_ocaml.Dom_html.window##requestAnimationFrame(
      Js_of_ocaml.Js.wrap_callback (fun (time:float) -> run time new_state )
    ) |> ignore 
  end

let init () = 
  draw_state State.init_state; run 0.0 State.init_state

let rec main () =
  (* sets intial game start page *)
  Graphics_js.set_color Resources.sand;
  Graphics_js.fill_rect 0 0 xDimension yDimension;
  Resources.draw_string Graphics.black 15 60 255 "set player names above";
  Graphics_js.set_text_size 25;
  Resources.gradient_text Resources.purple_grad 60 225 
    "PRESS ENTER TO PLAY" (Resources.size+5) |> ignore;
  Graphics_js.set_text_size 10;
  Resources.draw "desert" 10 80

let is_enter evt = 
  match evt##.keyCode with
  | 13  -> begin
      current_state := 
        {!current_state with 
         status = Playing; 
         camel1 = {(!current_state).camel1 with 
                   player_name = Resources.get_input_name One};
         camel2 = {(!current_state).camel2 with 
                   player_name = Resources.get_input_name Two};}; 
      draw_state !current_state; run 0.0 !current_state
    end
  | _ -> ()

let press_start evt =
  print_endline "in press start";
  match !current_state.status with 
  | Start | Paused -> let () = is_enter evt in Js_of_ocaml.Js._true
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

let _ = Js_of_ocaml.Dom_html.createAudio Js_of_ocaml.Dom_html.document

let _ = Js_of_ocaml.Dom_html.addEventListener Js_of_ocaml.Dom_html.document 
    Js_of_ocaml.Dom_html.Event.keyup (Js_of_ocaml.Dom_html.handler keyup) Js_of_ocaml.Js._true 

(* let _ = Js_of_ocaml.Dom_html.addEventListener Js_of_ocaml.Dom_html.document 
    Js_of_ocaml.Dom_html.Event. (Js_of_ocaml.Dom_html.handler pause_button) Js_of_ocaml.Js._true  *)

let () = main () 