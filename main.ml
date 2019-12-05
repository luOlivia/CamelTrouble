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

let () = print_endline (State.init_state.maze |> Maze.to_str)

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
  (* set_color (Graphics_js.rgb 255 251 224);
     fill_rect 0 0 xDimension yDimension; *)

  (* draws walls *)
  let halfw = State.wall_width/.2. in 
  let halfh = State.wall_height/.2. in 
  set_color (Graphics_js.rgb 4 145 23);

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
  draw_camel state.camel1 Graphics_js.black;
  draw_camel state.camel2 Graphics_js.black;

  (* draw balls *)
  draw_balls state Graphics_js.black

let keypressed evt =
  let () = match evt##keyCode with
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
  let () = match evt##keyCode with
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


(* let input state = 
   if not (Graphics_js.key_pressed ()) then State.update_state state
   else let k = Graphics_js.read_key () in
    flush_kp ();
    let state' = State.update_state state in
    match k with
    | '0' -> exit 0
    | 'w' -> State.move `Forward state' state.camel1
    | 'a' -> State.rotate `Left state' state.camel1
    | 's' -> State.move `Reverse state' state.camel1
    | 'd' -> State.rotate `Right state' state.camel1
    | 'e' -> State.shoot state'.camel1 state'
    | 'i' -> State.move `Forward state' state.camel2
    | 'j' -> State.rotate `Left state' state.camel2
    | 'k' -> State.move `Reverse state' state.camel2
    | 'l' -> State.rotate `Right state' state.camel2
    | 'u' -> State.shoot state'.camel2 state'
    | _ -> state' *)

let input d = d 
(** [run] displays the game window and allows users to quit with key 0
    refactor later bc alex *)
let rec run state =
  let new_state = input state in
  Graphics_js.auto_synchronize false;
  Graphics_js.clear_graph (); 
  draw_state new_state;
  Graphics_js.auto_synchronize true;
  run new_state



(* let init = Graphics_js.open_graph "";
   set_window_title "Camel Trouble";
   resize_window (State.xDimension) (State.yDimension);
   draw_state State.init_state;
   run State.init_state  *)

let init () = 
  draw_state State.init_state; run State.init_state

let main () = 
  (* Graphics_js.open_graph ""; *)
  set_window_title "Camel Trouble";
  resize_window (State.xDimension) (State.yDimension);
  Graphics_js.set_text_size 300; 
  Graphics_js.moveto 50 500;
  Graphics_js.draw_string "WELCOME TO";
  Resources.draw "desert" 10 300;
  Graphics_js.draw_string "PRESS ANY KEY TO PLAY";
  match Graphics_js.read_key () with 
  | _ -> init ()

let _ = Js_of_ocaml.Js.Opt.iter
    (Js_of_ocaml.Dom_html.CoerceTo.canvas (Js_of_ocaml.Dom_html.getElementById "canvas2"))
    Graphics_js.open_canvas

(* let c1 =
   Dom_html.addEventListener
    Dom_html.document
    Dom_html.Event.keydown
    (Dom_html.handler (fun ev ->
         let _ = match ev##.keyCode with
           | 32 -> input := Board.HardDrop
           | 90 -> input := Board.Rotate(false)
           | 88|38 -> input := Board.Rotate(true)
           | 67 -> input := Board.Swap
           | 78|37 -> input := Board.Translate(false)
           | 77|39 -> input := Board.Translate(true)
           | 66|40 -> input := Board.FastDrop
           | _ -> ();
         in
         Js._true ))
    Js._true
*)


(* let () = main () *)