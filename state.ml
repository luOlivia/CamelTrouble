open Camel
open Ball
open Position
open Maze
open Utils

type status = Start | Playing | Paused 

type t = 
  {
    ball_list: Ball.t list;
    camel1 : Camel.t;
    camel2 : Camel.t;
    camel1_alive : bool;
    camel2_alive : bool;
    game_end : bool;
    maze: Maze.t;
    status: status
  }

(** [axis] represents an axix of the two dimensions of [X] and [Y] *)
type axis = X | Y

type movement = Forward | Reverse

type rotation = CounterClockwise | Clockwise

let camel_width = 26.0
let ball_width = 10.0
let wall_width = 8.0
let wall_height = 50.0
let square_width = wall_height
let xDimension = 8.0 *. wall_width +. 7.0 *. square_width |> int_of_float
let yDimension = xDimension

let death_sound = Resources.audio "oof"
let shoot_sound = Resources.audio "pew"

let any_wall maze x y =
  Maze.is_wall_above maze x y
  || Maze.is_wall_below maze x y
  || Maze.is_wall_left maze x y
  || Maze.is_wall_right maze x y

(** [grid_to_pixel n]  *)
let grid_to_pixel n = 
  wall_width *. (n+.1.0) +. square_width*.n +. square_width/.2.0

let reinit_camel st x y player = 
  Camel.init player 
    (x |> float_of_int |> grid_to_pixel) (y |> float_of_int |> grid_to_pixel)
    begin match player with
      | One -> st.camel1.score + (if st.camel1_alive then 1 else 0)
      | Two -> st.camel2.score + (if st.camel2_alive then 1 else 0)
    end

let reinit_state st camel1 camel2 =
  {st with camel1; camel2; game_end = true; ball_list = []}

let rec reinit st = 
  try begin
    let x1 = ref (Random.int Maze.num_grid_squares) in 
    let y1 = ref (Random.int Maze.num_grid_squares) in 
    while any_wall st.maze !x1 !y1 do 
      x1 := (Random.int Maze.num_grid_squares);
      y1 := (Random.int Maze.num_grid_squares);
    done;

    let x2 = ref (Random.int Maze.num_grid_squares) in 
    let y2 = ref (Random.int Maze.num_grid_squares) in 
    while any_wall st.maze !x2 !y2 || (!x1 = !x2 && !y1 = !y2) do
      x2 := (Random.int Maze.num_grid_squares);
      y2 := (Random.int Maze.num_grid_squares);
    done;
    death_sound ();
    reinit_state st (reinit_camel st !x1 !y1 One) (reinit_camel st !x2 !y2 Two)
  end
  with 
  | _ -> reinit st

let current_square axis pos =
  let coord = match axis with
    | X -> pos.x
    | Y -> pos.y in

  let found = ref false in
  let out = ref 0.0 in

  for i = 0 to Maze.num_grid_squares-1 do
    let idx = i |> float_of_int in
    if (
      (not !found) 
      && (wall_width*.(idx+.1.0) +. square_width*.idx < coord)
      && ((wall_width*.(idx+.1.0) +. square_width*.(idx+.1.0)) > coord)
    )
    then begin 
      out := idx; 
      found := true; 
    end
  done;

  for i = 0 to Maze.num_grid_squares do
    let idx = i |> float_of_int in
    if (
      (not !found) 
      && (wall_width*.idx +. square_width*.idx <= coord)
      && ((wall_width*.(idx+.1.0) +. square_width*.(idx+.1.0)) > coord)
    )
    then begin
      out := idx -. 0.5;
      found := true;
    end
  done;

  if !found then !out else 0.0 

let horiz_collide st pos width =
  let cur_y = current_square Y pos in
  let t_cur_y = cur_y |> truncate in
  let y_wall = if cur_y <> t_cur_y then 1.0 else 0.0 in

  let w = width/.2.0 in
  let in_top_wall = 
    pos.y -. w <= (t_cur_y*.square_width) +. (t_cur_y +. 1.0)*.wall_width in
  let in_bottom_wall = 
    pos.y +. w >= (t_cur_y +. y_wall +. 1.0)*.(square_width +. wall_width) in

  let i_cur_x = current_square X pos |> int_of_float in
  let i_cur_y = cur_y |> int_of_float in

  let by_top_wall = Maze.is_wall_above st.maze i_cur_x i_cur_y in
  let by_bottom_wall = Maze.is_wall_below st.maze i_cur_x i_cur_y in

  (by_top_wall && in_top_wall) || (by_bottom_wall && in_bottom_wall)


let vert_collide st pos width =
  let cur_x = current_square X pos in
  let t_cur_x = cur_x |> truncate in
  let x_wall = if cur_x <> t_cur_x then 1.0 else 0.0 in

  let w = width/.2.0 in
  let in_left_wall = 
    pos.x -. w <= (t_cur_x*.square_width) +. (t_cur_x +. 1.0)*.wall_width in
  let in_right_wall = 
    pos.x +. w >= (t_cur_x +. x_wall +. 1.0)*.(square_width +. wall_width) in

  let i_cur_x = cur_x |> int_of_float in
  let i_cur_y = current_square Y pos |> int_of_float in

  let by_left_wall = Maze.is_wall_left st.maze i_cur_x i_cur_y in
  let by_right_wall = Maze.is_wall_right st.maze i_cur_x  i_cur_y in

  (by_left_wall && in_left_wall) || (by_right_wall && in_right_wall)

let corner_count pos axis = 
  let a = ref 0.0 in 
  let a_count = 
    begin match axis with 
      | X -> pos.x
      | Y -> pos.y
    end |> truncate |> ref in

  while !a_count > (wall_width +. square_width/.2.0) do
    a_count := !a_count -. (wall_width +. square_width);
    a := !a +. 1.;
  done;
  !a

let in_corner x y pos width =
  let w = wall_width +. square_width in
  let p1 = Position.init (x*.w) (y*.w) in
  let p2 = Position.init (x*.w+.wall_width) (y*.w) in
  let p3 = Position.init (x*.w) (y*.w+.wall_width) in
  let p4 = Position.init (x*.w+.wall_width) (y*.w+.wall_width) in
  let ds = List.map (distance pos) [p1;p2;p3;p4] in
  let dist = List.fold_left min max_float ds in
  dist < width /. 2.0 

let corner_collide st pos width =
  let x = corner_count pos X in
  let y = corner_count pos Y in

  let ix = x |> int_of_float in 
  let iy = y |> int_of_float in 
  let is_wall_in_corner = 
    ref (Maze.is_wall_above st.maze ix iy 
         || Maze.is_wall_left st.maze ix iy) in

  if x > 0.0 
  then is_wall_in_corner := 
      (!is_wall_in_corner || Maze.is_wall_above st.maze (ix-1) iy);
  if y > 0.0 
  then is_wall_in_corner := 
      (!is_wall_in_corner || Maze.is_wall_left st.maze ix (iy-1));

  if not !is_wall_in_corner then false else
    in_corner x y pos width

let count_ball_owners player balls =
  let f = match player with 
    | One -> fun a x -> if x.owner.player_num = One then 1 else 0
    | Two -> fun a x -> if x.owner.player_num = Two then 1 else 0
  in List.fold_left f 0 balls

let remove_balls st =
  let alive_balls = List.filter (fun x -> x.timer > 0.0) st.ball_list in
  let expired_balls = difference st.ball_list alive_balls in 
  let player_one_count = count_ball_owners One expired_balls in
  let player_two_count = count_ball_owners Two expired_balls in
  let camel1' = 
    {st.camel1 with num_balls = st.camel1.num_balls-player_one_count} in 
  let camel2' = 
    {st.camel2 with num_balls = st.camel2.num_balls-player_two_count} in 
  {st with 
   ball_list=alive_balls; 
   camel1=camel1'; 
   camel2=camel2'}

let is_collision ball camel =
  distance camel.pos ball.position <= camel_width/.2.0 +. ball_width/.2.0

let kill st = function
  | One -> {
      st with 
      camel1_alive = false; 
      ball_list = []; 
      maze = Maze.make_maze Maze.density
    }
  | Two -> {
      st with 
      camel2_alive = false; 
      ball_list = []; 
      maze = Maze.make_maze Maze.density
    }

let handle_death_collision ball st =
  if is_collision ball st.camel1
  then kill st One |> reinit
  else if is_collision ball st.camel2
  then kill st Two |> reinit
  else st

let handle_horiz_ball b = 
  let b' = Ball.flip_h b in
  let x' = Ball.new_pos_x b' in
  let p = Position.init x' b'.position.y in
  {b' with position = p} |> step_timer

let handle_vert_ball b = 
  let b' = Ball.flip_v b  in
  let y' = Ball.new_pos_y b' in
  let p = Position.init b'.position.x y' in
  {b' with position = p} |> step_timer

let handle_corner_ball b =
  if (current_square Y b.position) = (current_square Y b.position |> truncate) 
  then
    handle_horiz_ball b
  else
    handle_vert_ball b

let move_ball st b =
  let x = Ball.new_pos_x b in
  let y = Ball.new_pos_y b in  
  let next_point = Position.init x y in
  if horiz_collide st next_point ball_width then
    handle_horiz_ball b
  else if vert_collide st next_point ball_width then
    handle_vert_ball b
  else if corner_collide st next_point ball_width then
    handle_corner_ball b
  else
    {b with position = next_point} |> step_timer

let horiz_pos camel speed = 
  {camel.pos with x = Camel.move_horiz camel.pos.x camel.dir speed}

let vert_pos camel speed = 
  {camel.pos with y = Camel.move_vert camel.pos.y camel.dir speed}

let any_collision st pos width =
  corner_collide st pos width 
  || horiz_collide st pos width
  || vert_collide st pos width

let translated_pos st default_pos h_pos v_pos width = 
  if any_collision st h_pos width
  then begin
    if any_collision st v_pos width
    then default_pos 
    else v_pos
  end
  else h_pos

let after_corner st prev_pos next_pos width = 
  if corner_collide st prev_pos camel_width
  then next_pos
  else prev_pos

(* let move_camel st camel speed =
   let new_pos = Position.init 
      (Camel.move_horiz camel.pos.x camel.dir speed)
      (Camel.move_vert camel.pos.y camel.dir speed) in
   let h_pos = horiz_pos camel speed in 
   let v_pos = vert_pos camel speed in
   let t_pos = translated_pos st new_pos h_pos v_pos camel_width in
   let c_pos = after_corner st new_pos t_pos camel_width in
   if horiz_collide st c_pos camel_width && 
     vert_collide st c_pos camel_width
   then camel.pos (* collision, don't move *)
   else c_pos *)

let move_camel st camel speed =
  let new_pos = Position.init 
      (Camel.move_horiz camel.pos.x camel.dir speed)
      (Camel.move_vert camel.pos.y camel.dir speed) in
  let after_corner = if corner_collide st new_pos camel_width
    then
      let h_pos = {new_pos with x = Camel.move_horiz new_pos.x camel.dir speed} in
      if (corner_collide st h_pos camel_width 
          || horiz_collide st h_pos camel_width
          || vert_collide st h_pos camel_width)
      then
        let v_pos = {h_pos with y = Camel.move_vert new_pos.y camel.dir speed} in
        if (corner_collide st v_pos camel_width 
            || horiz_collide st v_pos camel_width 
            || vert_collide st v_pos camel_width)
        then new_pos else v_pos
      else h_pos
    else new_pos in
  let after_walls = if (horiz_collide st after_corner camel_width && 
                        vert_collide st after_corner camel_width)
    then camel.pos (* collision, don't move *)
    else
    if horiz_collide st after_corner camel_width then {camel.pos with x = Camel.move_horiz camel.pos.x camel.dir speed}
    else if vert_collide st after_corner camel_width then {camel.pos with y = Camel.move_vert camel.pos.y camel.dir speed}
    else after_corner
  in after_walls

let move_fwd_collide st camel = 
  {camel with pos = move_camel st camel Camel.fwd_speed}

let move_rev_collide st camel = 
  {camel with pos = move_camel st camel Camel.rev_speed}

let shoot camel st = 
  let curr_time = Unix.gettimeofday () in 
  if curr_time -. camel.shot_time < 0.25 then st
  else 
    begin 
      let camel = {camel with shot_time = curr_time} in
      if camel.num_balls >= 5 then st else
        begin
          shoot_sound ();
          let xpos = camel.pos.x +. ((ball_width /. 4.0 +. camel_width /. 2.0) *. cosine (90.0 -. camel.dir)) in
          let ypos = camel.pos.y -. ((ball_width /. 4.0 +. camel_width /. 2.0) *. sine (90.0 -. camel.dir)) in
          let new_pos = Position.init xpos ypos in
          if vert_collide st new_pos ball_width || horiz_collide st new_pos ball_width || corner_collide st new_pos ball_width then
            kill st camel.player_num |> reinit
          else
            let newball = Ball.init camel camel.dir (xpos) (ypos) in
            let camel' = {camel with num_balls=camel.num_balls+1} in
            let st' = {st with ball_list=(newball::st.ball_list)} in
            match camel.player_num with
            | One -> {st' with camel1=camel'}
            | Two -> {st' with camel2=camel'}
        end
    end 

(** returns new state with camel moved positions *)
let move direction st camel =
  let new_camel = match direction with
    | Forward -> move_fwd_collide st camel
    | Reverse -> move_rev_collide st camel in
  match camel.player_num with 
  | One -> {st with camel1 = new_camel}
  | Two -> {st with camel2 = new_camel}

let rotate d st camel =
  (* let new_camel = move_fwd_collide state camel in  *)
  let new_camel = match d with
    | CounterClockwise -> Camel.turn_left camel
    | Clockwise -> Camel.turn_right camel in
  match camel.player_num with 
  | One -> {st with camel1 = new_camel}
  | Two -> {st with camel2 = new_camel}



(**[check_death st balls] is the new state after checking if any ball collides with camels.*)
let rec check_death st aux_balls all_balls =
  match aux_balls with
  | [] -> begin
      {st with ball_list = all_balls} end
  | ball::t -> if (is_collision ball st.camel1 || is_collision ball st.camel2)
    then (handle_death_collision ball st)
    else (check_death st t all_balls)


(**[move_all_balls player st] is the new state after all balls have been moved.*)
let move_all_balls st =
  (* let rec iter_balls blst = function
     | [] -> blst
     | ball::t -> iter_balls ((State.move_ball st ball)::blst) t
     in let blst' = iter_balls [] (st.ball_list) in  *)
  (* let blst' = List.fold_left (fun a x -> (move_ball st x)::a) [] st.ball_list in
     check_death st blst' *)
  let blst' = (List.fold_left (fun a x -> (move_ball st x)::a) [] st.ball_list) in
  let st' = {st with ball_list = blst'} in
  let st'' = remove_balls st' in
  check_death st'' st''.ball_list st''.ball_list

(** [rot_point x y center_x center_y angle] is the
    point (x,y) rotated around center pt by angle *)
let rot_point x y center_x center_y angle =
  let x' = x -. center_x in
  let y' = y -. center_y in
  let rot_x = x'*.(cosine angle) -. y'*.(sine angle) in
  let rot_y = x'*.(sine angle) +. y'*.(cosine angle) in
  (rot_x +. center_x |> int_of_float, rot_y +. center_y |> int_of_float)

let rec update_state state =
  move_all_balls state

let init_camel1 = Camel.init One 0.0 0.0 ~-1
let init_camel2 = Camel.init Two 0.0 0.0 ~-1

let init_state = {
  ball_list = [];
  camel1 = init_camel1;
  camel2 = init_camel2;
  camel1_alive = true;
  camel2_alive = true;
  game_end = false;
  maze = Maze.make_maze Maze.density;
  status = Start; 
} |> reinit

