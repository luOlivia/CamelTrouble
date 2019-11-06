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

let draw_camel camel color = 
  set_color color;
  let curr_x = int_of_float camel.pos.x in 
  let curr_y = int_of_float camel.pos.y in 
  let c_width = int_of_float State.camel_width in 
  fill_ellipse curr_x curr_y 10 20

(** returns new state with camel moved positions *)
let move_fwd st speed =
  (* let new_camel = move_fwd state camel in  *)
  let new_camel = Camel.free_range st.camel1 speed in 
  print_endline (Camel.print new_camel);
  draw_camel st.camel1 Graphics.white;
  draw_camel new_camel Graphics.black;
  {st with camel1 = new_camel}

let rotate st =
  (* let new_camel = move_fwd state camel in  *)
  let new_camel = Camel.turn_left st.camel1 in 
  print_endline (Camel.print new_camel);
  draw_camel st.camel1 Graphics.white;
  draw_camel new_camel Graphics.black;
  {st with camel1 = new_camel}

let input state = 
  match read_key () with
  | 'q' -> print_endline "exit"; exit 0
  | 'w' -> print_endline "player 1 up"; move_fwd state (-20.0)
  | 'a' -> print_endline "player 1 left"; rotate state
  | 's' -> print_endline "player 1 down"; move_fwd state 10.0
  | 'd' -> print_endline "player 1 right"; state
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

