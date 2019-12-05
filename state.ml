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

let rec print_list = function 
    [] -> ()
  | e::l -> print_int e ; print_string " " ; print_list l

let camel_width = 26.0
let ball_width = 6.0
let wall_width = 5.0
let wall_height = 45.5
let square_width = 50.0
let xDimension = 8.0 *. wall_width +. 7.0 *. square_width |> int_of_float
let yDimension = xDimension


let grid_to_pixel n = 
  wall_width *. (n+.1.0) +. square_width*.n +. square_width/.2.

let reinit st = 
  let x1 = ref (Random.int Maze.num_grid_squares) in 
  let y1 = ref (Random.int Maze.num_grid_squares) in 
  let x2 = ref (Random.int Maze.num_grid_squares) in 
  let y2 = ref (Random.int Maze.num_grid_squares) in 
  while ((Maze.is_wall_above st.maze !x1 !y1 || Maze.is_wall_below st.maze !x1 !y1 || Maze.is_wall_left st.maze !x1 !y1
          || Maze.is_wall_right st.maze !x1 !y1)) do 
    x1 := (Random.int Maze.num_grid_squares);
    y1 := (Random.int Maze.num_grid_squares);
  done;
  while ((Maze.is_wall_above st.maze !x2 !y2 || Maze.is_wall_below st.maze !x2 !y2 || Maze.is_wall_left st.maze !x2 !y2
          || Maze.is_wall_right st.maze !x2 !y2) && (!x1 = !x2 && !y1 = !y2)) do 
    x2 := (Random.int Maze.num_grid_squares);
    y2 := (Random.int Maze.num_grid_squares);
  done;
  print_endline ("x2 position: "^(string_of_int !x2));
  print_endline ("x1 position: "^(string_of_int !x1));
  print_endline ("y2 position: "^(string_of_int !y2));
  print_endline ("y1 position: "^(string_of_int !y1));
  (* let camel1' = Camel.init 1 (((float_of_int !x1)/. 7.)*. (float_of_int xDimension)) (((float_of_int !y1)/. 7.)*. (float_of_int yDimension)) in 
     let camel2' = Camel.init 2 (((float_of_int !x2)/. 7.)*. (float_of_int xDimension)) (((float_of_int !y2)/. 7.)*. (float_of_int yDimension)) in  *)
  let camel1' = Camel.init 1 (grid_to_pixel (float_of_int !x1)) (grid_to_pixel (float_of_int !y1)) in
  let camel2' = Camel.init 2 (grid_to_pixel (float_of_int !x2)) (grid_to_pixel (float_of_int !y2)) in 
  let st' = {st with camel1 = camel1'} in  {st' with camel2 = camel2'}


(* make_position (Random.int x |> float_of_int) (Random.int y |> float_of_int)  *)


(** [to_radians x] is degrees [x] to radians *)
let to_radians x = x *. Float.pi /. 180.0
(** [cosine degree] is cosine of degrees *)
let cosine degree = degree |> to_radians |> Stdlib.cos
(** [sine degree] is sine of degrees *)
let sine degree = degree |> to_radians |> Stdlib.sin

(*TODO returns new state with camel1 or camel2 modified
  originally from: ball *)
(* let remove_ball b =
   failwith "unimplemented" *)
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
  if !found then !out else -1.0

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
  let by_top_wall = Maze.is_wall_above st.maze icurx icury in
  let in_top_wall = pos.y -. w <= (rcury *. square_width) +. (rcury+.1.0)*.wall_width in
  let by_bottom_wall = Maze.is_wall_below st.maze icurx icury in
  let in_bottom_wall = pos.y +. w >= (rcury+.1.0+.ywall)*.(square_width +. wall_width) in
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
  (* print_endline ("vert x: "^(string_of_float curx));
     print_endline ("vert y: "^(string_of_float cury)); *)
  let by_left_wall = Maze.is_wall_left st.maze icurx icury in
  let in_left_wall = pos.x -. w <= (rcurx *. square_width) +. (rcurx+.1.0)*.wall_width in
  let by_right_wall = Maze.is_wall_right st.maze icurx icury in
  let in_right_wall = pos.x +. w >= (rcurx+.1.0+.xwall)*.(square_width +. wall_width) in
  (* print_endline ("vert collide: "^(string_of_bool ((by_left_wall && in_left_wall) || (by_right_wall && in_right_wall)))); *)
  (by_left_wall && in_left_wall) || (by_right_wall && in_right_wall)

(** TODO *)
let corner_collide st pos width =
  let w = width /. 2.0 in
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
  (* print_endline ("corner x: "^(string_of_int !x));
     print_endline ("corner y: "^(string_of_int !y)); *)
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
(* to-do: remove [ball] from st *)
let remove_balls balls =
  List.filter (fun x -> x.timer > 0.0) balls

(** TO-DO: determines if bullet & camel collides based on position*)
let collision bullet camel =
  let dist = distance camel.pos bullet.position in
  dist <= camel_width/.2.0 +. ball_width/.2.0

(** TO-DO: returns new state depending on bullet collision with 
    camel 1 or 2. Also generates new maze*)
let handle_collision bullet st =
  if collision bullet st.camel1
  then let st' = {st with camel1_alive = true; ball_list = []; maze = Maze.make_maze Maze.density}
    in reinit st'
  else begin if collision bullet st.camel2
    then let st' = {st with camel2_alive = true; ball_list = []; maze = Maze.make_maze Maze.density}
      in reinit st'
    else st
  end

let move_ball st b =
  let new_x = Ball.new_ball_pos_x b in
  let new_y = Ball.new_ball_pos_y b in  
  let next_point = make_position new_x new_y in
  if horiz_collide st next_point ball_width then
    (* let fball = Ball.flip_ball_h {b with position = next_point} in  *)
    let fball = Ball.flip_ball_h b in
    let nx = Ball.new_ball_pos_x fball in
    (* let np = make_position nx new_y in  *)
    let np = make_position nx fball.position.y in
    {fball with position = np; timer = fball.timer -. 0.1}
  else if vert_collide st next_point ball_width then
    let fball = Ball.flip_ball_v b  in
    let ny = Ball.new_ball_pos_y fball in
    let np = make_position fball.position.x ny in
    {fball with position = np; timer = fball.timer -. 0.1}
  else if corner_collide st next_point ball_width then
    if (current_square `Y b.position) = ((current_square `Y b.position) |> truncate) then
      let fball = Ball.flip_ball_h b in
      let nx = Ball.new_ball_pos_x fball in
      let np = make_position nx fball.position.y in
      {fball with position = np; timer = fball.timer -. 0.1}
    else
      let fball = Ball.flip_ball_v b in
      let ny = Ball.new_ball_pos_y fball in
      let np = make_position fball.position.x ny in
      {fball with position = np; timer = fball.timer -. 0.1}
      (* else if collision b st.camel1 || collision b st.camel2 then
         begin print_endline ("HIIIIIII THERE IS A CAMEL COLLISION");
          (* camel_collision b st *){b with position = next_point}
         end   *)
  else
    {b with position = next_point; timer = b.timer -. 0.1}



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

let shoot camel st =
  (* if camel.num_bullets >= 5 then st else *)
  begin
    let xpos = camel.pos.x +. ((ball_width /. 2.0 +. camel_width /. 2.0) *. cosine (90.0 -. camel.dir)) in
    let ypos = camel.pos.y -. ((ball_width /. 2.0 +. camel_width /. 2.0) *. sine (90.0 -. camel.dir)) in
    let new_pos = make_position xpos ypos in
    if vert_collide st new_pos ball_width || horiz_collide st new_pos ball_width || corner_collide st new_pos ball_width then
      match camel.player_num with
      | 1 -> {st with camel1_alive=false}
      | 2 -> {st with camel2_alive=false}
      | _ -> failwith "that many players not allowed"
    else
      let newball = Ball.init camel camel.dir (xpos) (ypos) in
      let camel' = {camel with num_bullets=camel.num_bullets+1} in
      let st' = {st with ball_list=(newball::st.ball_list)} in
      print_endline "updated state";
      match camel.player_num with
      | 1 -> {st' with camel1=camel'}
      | 2 -> {st' with camel2=camel'}
      | _ -> failwith "that many players not allowed"
  end


(** returns new state with camel moved positions *)
let move direction st =
  let new_camel = match direction with
    | `Forward -> move_fwd st st.camel1
    | `Reverse -> move_rev st st.camel1 in
  print_endline (Camel.to_str new_camel);
  let st' = {st with camel1 = new_camel} in st'

let rotate d st =
  (* let new_camel = move_fwd state camel in  *)
  let new_camel = match d with
    | `Left -> Camel.turn_left st.camel1
    | `Right -> Camel.turn_right st.camel1 in
  print_endline (Camel.to_str new_camel);
  let st' = {st with camel1 = new_camel} in st'


(**[check_death st balls] is the new state after checking if any ball collides with camels.*)
let rec check_death st aux_balls all_balls =
  match aux_balls with
  | [] -> begin (*print_endline "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~finished checking death bitch";*)
      {st with ball_list = all_balls} end
  | ball::t -> if (collision ball st.camel1 || collision ball st.camel2)
    then (print_endline "*******************************************we are in check_death";
          handle_collision ball st)
    else (check_death st t all_balls)

(* let rec check_death' st balls = *)

(* timer && owner
   let remove_balls st balls = *)



(**[move_all_balls player st] is the new state after all balls have been moved.*)
let move_all_balls st =
  (* let rec iter_balls blst = function
     | [] -> blst
     | ball::t -> iter_balls ((State.move_ball st ball)::blst) t
     in let blst' = iter_balls [] (st.ball_list) in  *)
  (* let blst' = List.fold_left (fun a x -> (move_ball st x)::a) [] st.ball_list in
     check_death st blst' *)
  let blst' = (List.fold_left (fun a x -> (move_ball st x)::a) [] st.ball_list
               |> remove_balls) in
  check_death st blst' blst'

(** [rot_point x y center_x center_y angle] is the
    point (x,y) rotated around center pt by angle *)
let rot_point x y center_x center_y angle =
  let x' = x -.center_x in
  let y' = y -.center_y in
  let rot_x = x'*.(cosine angle) -. y'*.(sine angle) in
  let rot_y = x'*.(sine angle) +. y'*.(cosine angle) in
  (rot_x +. center_x |> int_of_float, rot_y +. center_y |> int_of_float)



let rec update_state state =
  move_all_balls state

let camel1 = Camel.init 1 120.0 120.0
let camel2 = Camel.init 2 220.0 220.0


let init_state = {
  ball_list= [];
  camel1= camel1;
  camel2= camel2;
  camel1_alive= true;
  camel2_alive= true;
  maze = Maze.make_maze Maze.density
}