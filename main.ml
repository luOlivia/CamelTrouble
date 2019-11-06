open Camel
open State
open Ball
open Maze
open Graphics

let camel1 = Camel.init "camel1" 120.0 120.0
let camel2 = Camel.init "camel2" 620.0 420.0
let state = {
  ball_list= [];
  camel1= camel1;
  camel2= camel2;
  camel1_alive= true;
  camel2_alive= true; 
  maze= make_maze 100
} 

(** [to_radians x] is degrees [x] to radians *)
let to_radians x = x *. Float.pi /. 180.0

let cosine degree = degree |> to_radians |> Stdlib.cos
let sine degree = degree |> to_radians |> Stdlib.sin

let draw_camel camel color =
  set_color color;
  let w = 10.0 in
  let h = 20.0 in
  let d = camel.dir in
  let cx = camel.pos.x in 
  let cy = camel.pos.y in
  let tl_x = cx +. (cx-.w)*.(cosine d) -. (cy+.h)*.(sine d) |> int_of_float in 
  let tl_y = cy +.(cx-.w)*.(sine d) +. (cy+.h)*.(cosine d) |> int_of_float in 
  let tr_x = cx +.(cx+.w)*.(cosine d) -. (cy+.h)*.(sine d) |> int_of_float in 
  let tr_y = cy +.(cx+.w)*.(sine d) +. (cy+.h)*.(cosine d) |> int_of_float in 
  let br_x = cx +.(cx+.w)*.(cosine d) -. (cy-.h)*.(sine d) |> int_of_float in 
  let br_y = cy +.(cx+.w)*.(sine d) +. (cy-.h)*.(cosine d) |> int_of_float in 
  let bl_x = cx +.(cx-.w)*.(cosine d) -. (cy-.h)*.(sine d) |> int_of_float in 
  let bl_y = cy +.(cx-.w)*.(sine d) +. (cy-.h)*.(cosine d) |> int_of_float in 
  print_endline ("points "^"("^string_of_int tl_x^","^string_of_int tl_y^")");
  fill_poly [|(tl_x, tl_y);(tr_x, tr_y);(br_x, br_y);(bl_x, bl_y) |]

(* let draw_camel camel color = 
   set_color color;
   let curr_x = int_of_float camel.pos.x in 
   let curr_y = int_of_float camel.pos.y in 
   fill_ellipse curr_x curr_y 10 20 *)

(** returns new state with camel moved positions *)
let move_fwd st speed =
  (* let new_camel = move_fwd state camel in  *)
  let new_camel = Camel.free_range st.camel1 speed in 
  print_endline (Camel.print new_camel);
  draw_camel st.camel1 Graphics.white;
  draw_camel new_camel Graphics.black;
  {st with camel1 = new_camel}

let rotate d st =
  (* let new_camel = move_fwd state camel in  *)
  let new_camel = if d = "left" then Camel.turn_left st.camel1 
    else Camel.turn_right st.camel1 in
  print_endline (Camel.print new_camel);
  draw_camel st.camel1 Graphics.white;
  draw_camel new_camel Graphics.black;
  {st with camel1 = new_camel}

let input state = 
  match read_key () with
  | 'q' -> print_endline "exit"; exit 0
  | 'w' -> print_endline "player 1 up"; move_fwd state (-5.0)
  | 'a' -> print_endline "player 1 left"; rotate "right" state
  | 's' -> print_endline "player 1 down"; move_fwd state 5.0
  | 'd' -> print_endline "player 1 right"; rotate "left" state
  | _ -> print_endline "sir pls"; state

(** [run] displays the game window and allows users to quit with key q *)
let rec run state =
  let new_state = input state in
  run new_state


let init = Graphics.open_graph "";
  set_window_title "Camel Trouble";
  resize_window (int_of_float State.square_width) (int_of_float State.square_width);
  draw_camel camel1 Graphics.black;
  draw_camel camel2 Graphics.black;
  run state

