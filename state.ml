open Camel 
open Ball
open Position
open Maze

type t = {
  ball_list: Ball.t list;
  camel1 : Camel.t;
  camel2 : Camel.t;
  camel1_alive : bool;
  camel2_alive : bool; 
  maze: Maze.t 
} 

let camel_width = 26.0
let ball_width = 6.0 
let wall_width = 5.0
let wall_height = 55.5
let square_width = 50.0

(*TODO returns new state with camel1 or camel2 modified
  originally from: ball *)
let remove_ball b = 
  failwith "unimplemented"
(* if b.owner = 0 then b.state.camel1.dec_ball_count
   else b.state.camel2.dec_ball_count  *)

let current_square x_or_y pos = 
  let coord = match x_or_y with 
    | `X -> pos.x
    | `Y -> pos.y in
  let found = ref false in
  let out = ref 0.0 in 
  for i = 0 to Maze.num_grid_squares-1 do
    let ix = float_of_int i in 

    if ((not !found)&& (wall_width *. (ix+.1.0) +. square_width*.ix < coord)
        && (wall_width *. (ix+.1.0) +. square_width*.(ix+.1.0) > coord))
    then 
      (out := ix; 
       found := true;)
  done;
  for i = 0 to Maze.num_grid_squares do
    let ix = float_of_int i in 
    if ((not !found)&& (wall_width *.ix +. square_width*.ix <= coord)
        && (wall_width*.(ix+.1.0) +. square_width*.(ix+.1.0) > coord))
    then 
      (out := ix -. 0.5; 
       found := true;)
  done;
  if !found then !out else failwith "should not occur curr_square"

(* let current_x_square pos = 
   print_endline("NEW CALL");
   let coord = pos.x in 
   let found = ref false in
   let out = ref 0.0 in 
   for i = 0 to Maze.num_grid_squares-1 do
    let ix = float_of_int i in 

    if ((not !found)&& (wall_width *. (ix+.1.0) +. square_width*.ix < coord)
        && (wall_width *. (ix+.1.0) +. square_width*.(ix+.1.0) > coord))
    then 
      (print_endline ("1st bool xsquare "^string_of_bool((wall_width *. (ix+.1.0) +. square_width*.ix < coord)));
       print_endline ("2nd bool xsquare "^(string_of_bool((wall_width *. (ix+.1.0) +. square_width*.(ix+.1.0) > coord))));
       out := ix; found := true; 
       print_endline ("found 1 xsquare"^string_of_bool(!found));
       print_endline ("updated x " ^ string_of_float !out);)
   done;
   for i = 0 to Maze.num_grid_squares do
    let ix = float_of_int i in 
    if ((not !found)&& (wall_width *.ix +. square_width*.ix <= coord)
        && (wall_width*.(ix+.1.0) +. square_width*.(ix+.1.0) > coord))
    then 
      (print_endline ("1st bool xsquare' "^string_of_bool((wall_width *.ix +. square_width*.ix <= coord)));
       print_endline ("2nd bool xsquare' "^(string_of_bool((wall_width*.(ix+.1.0) +. square_width*.(ix+.1.0) > coord))));
       out := ix -. 0.5; found := true;
       print_endline ("updated x " ^ string_of_float !out);
       print_endline ("found 2 xsquare' "^string_of_bool(!found));)
   done;
   if !found then !out else failwith "should not occur"



   let current_y_square pos = 
   print_endline("NEW CALL");
   let coord = pos.y in 
   let found = ref false in
   let out = ref 0.0 in 
   for i = 0 to Maze.num_grid_squares-1 do
    let ix = float_of_int i in 

    if ((not !found) && (wall_width*.(ix+.1.0) +. square_width*.ix < coord)
        && (wall_width*.(ix+.1.0) +. square_width*.(ix+.1.0) > coord))
    then 
      (print_endline ("1st bool ysquare "^string_of_bool((wall_width *. (ix+.1.0) +. square_width*.ix < coord)));
       print_endline ("2nd bool ysquare "^string_of_bool((wall_width*.(ix+.1.0) +. square_width*.(ix+.1.0) > coord)));
       out := ix; 
       found := true; 
       print_endline ("found 1 ysquare "^string_of_bool(!found));
       print_endline ("updated y " ^ string_of_float !out);)
   done;


   for i = 0 to Maze.num_grid_squares do
    let ix = float_of_int i in 
    if ((not !found) &&(wall_width*.ix +. square_width*.ix <= coord)
        && (wall_width*.(ix+.1.0) +. square_width*.(ix+.1.0) > coord))
    then 
      (print_endline ("1st bool ysquare' "^string_of_bool((wall_width*.(ix+.1.0) +. square_width*.(ix+.1.0) > coord)));
       print_endline ("2nd bool ysquare' "^string_of_bool((wall_width*.(ix+.1.0) +. square_width*.(ix+.1.0) > coord)));
       out := ix -. 0.5; 
       found := true;
       print_endline ("found 2 ysquare' "^string_of_bool(!found));
       print_endline ("updated y " ^ string_of_float !out))
   done;
   if !found then (print_endline (string_of_float !out); !out) else failwith "should not occur" *)



(* let current_y_square coord = 
   let out = ref 0.0 in 
   for i = 0 to Maze.num_grid_squares-1 do
    let ix = float_of_int i in 
    if wall_width *. (ix+.1.0) +. square_width*.ix < coord
    && wall_width*.(ix+.1.0) +. square_width*.(ix+.1.0) > coord
    then out := ix
   done;
   for i = 0 to Maze.num_grid_squares-1 do
    let ix = float_of_int i in 
    if wall_width *. (ix+.1.0) +. square_width*.ix < coord
    && wall_width*.(ix+.1.0) +. square_width*.(ix+.1.0) > coord
    then out := ix -. 0.5 
   done; !out *)


(* let current_y_square pos =
   current_square pos.y

   (* return x value of square the center of tank is currently in *)
   let current_x_square pos =
   current_square pos.x *)

(**TODO [horiz_collide st pos width] is whether or not the next position of the object at 
   [pos] with a [width] intersects with a horizontal wall.*)

let truncate x = float_of_int (int_of_float x)

let horiz_collide st pos width = 
  let w = width/.2.0 in 
  let cury = current_square `Y pos in
  let curx = current_square `X pos in 
  let icury = int_of_float cury in 
  let icurx = int_of_float curx in 
  let rcury = truncate cury in 
  let ywall = if cury <> rcury then 1.0 else 0.0 in 
  print_endline ("horz x: "^(string_of_float curx));
  print_endline ("horz y: "^(string_of_float cury));
  let by_top_wall = Maze.is_wall_above st.maze icurx icury in 
  let in_top_wall = pos.y -. w <= (rcury *. square_width) +. (rcury+.1.0)*.wall_width in 
  let by_bottom_wall = Maze.is_wall_below st.maze icurx icury in 
  let in_bottom_wall = pos.y +. w >= (rcury+.1.0+.ywall)*.(square_width +. wall_width) in 
  print_endline ("horz collide: "^(string_of_bool ((by_top_wall && in_top_wall) || (by_bottom_wall && in_bottom_wall))));
  (by_top_wall && in_top_wall) || (by_bottom_wall && in_bottom_wall)


(**TODO *)
let vert_collide st pos width =
  let w = width/.2.0 in 
  let cury = current_square `Y pos in
  let curx = current_square `X pos in 
  let icury = int_of_float cury in 
  let icurx = int_of_float curx in 
  let rcurx = truncate curx in 
  let xwall = if curx <> rcurx then 1.0 else 0.0 in 
  print_endline ("vert x: "^(string_of_float curx));
  print_endline ("vert y: "^(string_of_float cury));
  let by_left_wall = Maze.is_wall_left st.maze icurx icury in 
  let in_left_wall = pos.x -. w <= (rcurx *. square_width) +. (rcurx+.1.0)*.wall_width in 
  let by_right_wall = Maze.is_wall_right st.maze icurx icury in 
  let in_right_wall = pos.x +. w >= (rcurx+.1.0+.xwall)*.(square_width +. wall_width) in 
  print_endline ("vert collide: "^(string_of_bool ((by_left_wall && in_left_wall) || (by_right_wall && in_right_wall))));
  (by_left_wall && in_left_wall) || (by_right_wall && in_right_wall)

(** TODO *)
let corner_collide st pos width = 
  let w = width/.2.0 in 
  let x = ref 0 in 
  let y = ref 0 in 
  let xcounter = ref (int_of_float pos.x) in 
  while !xcounter > (wall_width +. square_width/.2.0 |> int_of_float) do 
    xcounter := !xcounter - (int_of_float wall_width) - (int_of_float square_width);
    x := !x + 1;
  done;
  let ycounter = ref (int_of_float pos.y) in 
  while !ycounter > (wall_width +. square_width/.2.0 |> int_of_float)do 
    ycounter := !ycounter - (int_of_float wall_width) - (int_of_float square_width);
    y := !y + 1;
  done;
  print_endline ("corner x: "^(string_of_int !x));
  print_endline ("corner y: "^(string_of_int !y));
  let is_wall_in_corner = ref (Maze.is_wall_above st.maze !x !y || Maze.is_wall_left st.maze !x !y) in
  if !x > 0 then is_wall_in_corner := !is_wall_in_corner || Maze.is_wall_above st.maze (!x-1) !y; 
  if !y > 0 then is_wall_in_corner := !is_wall_in_corner || Maze.is_wall_left st.maze !x (!y-1);
  if not !is_wall_in_corner then false else
    let cx = !x |> float_of_int in 
    let cy = !y |> float_of_int in 
    let w_sq = wall_width +. square_width in 
    let p1 = make_position (cx*.w_sq) (cy*.w_sq) in 
    let p2 = make_position (cx*.w_sq+.wall_width) (cy*.w_sq) in 
    let p3 = make_position (cx*.w_sq) (cy*.w_sq+.wall_width) in 
    let p4 = make_position (cx*.w_sq+.wall_width) (cy*.w_sq+.wall_width) in 
    let d1 = distance p1 pos in 
    let d2 = distance p2 pos in 
    let d3 = distance p3 pos in 
    let d4 = distance p4 pos in 
    let dist = min d1 (min d2 (min d3 d4)) in 
    dist < w

(**
   - modifies ball 
*)

(* ---------------------BALL BOI--------------------- *)

let move_ball st b = 
  let new_x = Ball.new_ball_pos_x b in 
  let new_y = Ball.new_ball_pos_y b in  
  let next_point = make_position new_x new_y in 
  if horiz_collide st next_point ball_width then
    let fball = Ball.flip_ball_h {b with position = next_point} in 
    let nx = Ball.new_ball_pos_x fball in 
    let np = make_position nx new_y in 
    {fball with position = np} 
  else if vert_collide st next_point ball_width then 
    let fball = Ball.flip_ball_v {b with position = next_point}  in 
    let ny = Ball.new_ball_pos_y fball in 
    let np = make_position new_x ny in 
    {fball with position = np} 
  else if corner_collide st next_point ball_width then 
    if (current_square `Y b.position) = ((current_square `Y b.position) |> truncate) then
      let fball = Ball.flip_ball_h {b with position = next_point} in 
      let nx = Ball.new_ball_pos_x fball in 
      let np = make_position nx new_y in 
      {fball with position = np} 
    else 
      let fball = Ball.flip_ball_v {b with position = next_point}  in 
      let ny = Ball.new_ball_pos_y fball in 
      let np = make_position new_x ny in 
      {fball with position = np}
  else 
    {b with position = next_point} 


(* ---------------------CAMEL BOI--------------------- *)

let move_camel st camel speed = 
  let new_pos = {
    x= Camel.move_horiz camel.pos.x camel.dir speed;
    y= Camel.move_vert camel.pos.y camel.dir speed
  } in 
  let after_corner = if corner_collide st new_pos camel_width
    then 
      let h_pos = {new_pos with x = Camel.move_horiz new_pos.x camel.dir speed} in 
      if corner_collide st h_pos camel_width || horiz_collide st h_pos camel_width|| vert_collide st h_pos camel_width
      then 
        let v_pos = {h_pos with y = Camel.move_vert new_pos.y camel.dir speed} in 
        if corner_collide st v_pos camel_width || horiz_collide st v_pos camel_width || vert_collide st v_pos camel_width
        then new_pos else v_pos
      else h_pos
    else new_pos in 
  let after_walls = if horiz_collide st after_corner camel_width && vert_collide st after_corner camel_width
    then camel.pos (* collision, don't move *)
    else 
    if horiz_collide st after_corner camel_width then {camel.pos with x = Camel.move_horiz camel.pos.x camel.dir speed}
    else if vert_collide st after_corner camel_width then {camel.pos with y = Camel.move_vert camel.pos.y camel.dir speed}
    else after_corner
  in after_walls

let move_fwd st camel = {camel with pos = move_camel st camel Camel.fwd_speed}
let move_rev st camel = {camel with pos = move_camel st camel Camel.rev_speed}