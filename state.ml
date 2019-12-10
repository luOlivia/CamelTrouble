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

(** The following are constants for numerical calculations in the game *)
let camel_width = 26.0
let ball_width = 10.0
let wall_width = 8.0
let wall_height = 50.0
let square_width = wall_height
let xDimension = 8.0 *. wall_width +. 7.0 *. square_width |> int_of_float
let yDimension = xDimension

(** [death_sound] is the sound camels make when they die *)
let death_sound = Resources.audio "oof"
(** [shoot_sound] is the sound of camels shooting balls *)
let shoot_sound = Resources.audio "pew"

(** [any_wall maze x y] is true if the position [(x,y)]
    has a wall in [maze] around it *)
let any_wall maze x y =
  Maze.is_wall_above maze x y
  || Maze.is_wall_below maze x y
  || Maze.is_wall_left maze x y
  || Maze.is_wall_right maze x y

(** [grid_to_pixel n] is the pixel value of a grid [n] *)
let grid_to_pixel n = 
  wall_width *. (n+.1.0) +. square_width*.n +. square_width/.2.0

(** [reinit_camel st x y player] is the reinitialized camel of [player] in game
    state [st] at position [(x, y)] after a round has ended *)
let reinit_camel st x y player = 
  Camel.init player 
    (x |> float_of_int |> grid_to_pixel) (y |> float_of_int |> grid_to_pixel)
    begin match player with
      | One -> st.camel1.score + (if st.camel1_alive then 1 else 0)
      | Two -> st.camel2.score + (if st.camel2_alive then 1 else 0)
    end
    (Resources.get_input_name player)

(** [reinit_state st camel1 camel2] is the new state with reinitialized
    [camel1] and [camel2] after a game has ended*)
let reinit_state st camel1 camel2 =
  {st with camel1; camel2; game_end = true; ball_list = []}

(** [reinit st] is the new state after a game has ended*)
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

(** [first_square_loop out found coord] is a helper loop for calculating 
    current square *)
let first_square_loop out found coord = 
  for i = 0 to Maze.num_grid_squares-1 do
    let idx = i |> float_of_int in
    if (not !found) 
    && (wall_width*.(idx+.1.0) +. square_width*.idx < coord)
    && ((wall_width*.(idx+.1.0) +. square_width*.(idx+.1.0)) > coord)
    then begin 
      out := idx; 
      found := true; 
    end
  done

(** [second_square_loop out found coord] is a helper loop for calculating 
    current square *) 
let second_square_loop out found coord =
  for i = 0 to Maze.num_grid_squares do
    let idx = i |> float_of_int in
    if (not !found) 
    && (wall_width*.idx +. square_width*.idx <= coord)
    && ((wall_width*.(idx+.1.0) +. square_width*.(idx+.1.0)) > coord)
    then begin
      out := idx -. 0.5;
      found := true;
    end
  done 

            <<<<<<< HEAD
            (** [current_square axis pos] is the current maze square that the [pos] inhabits*)
            =======
            (**[current_square axis pos] is the current maze square that [pos] inhabits*)
            >>>>>>> 5e488b073c1e692c918dc096a8d22e210ca0b021
let current_square axis pos =
  let coord = match axis with
    | X -> pos.x
    | Y -> pos.y in
  let found = ref false in
  let out = ref 0.0 in

  first_square_loop out found coord;
  second_square_loop out found coord;

  if !found then !out else 0.0 

(** [horiz_collide st pos width] is [true] if the current [pos] with [width] 
    collides with a horizontal wall*)
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

(** [vert_collide st pos width] is [true] if the current [pos] with [width] 
    collides with a vertical wall*)
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

(** [corner_count pos axis] a helper function for corner_collide *)
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

(** [in_corner x y pos width] is [true] if this [pos] is in a corner *)
let in_corner x y pos width =
  let w = wall_width +. square_width in
  let p1 = Position.init (x*.w) (y*.w) in
  let p2 = Position.init (x*.w+.wall_width) (y*.w) in
  let p3 = Position.init (x*.w) (y*.w+.wall_width) in
  let p4 = Position.init (x*.w+.wall_width) (y*.w+.wall_width) in
  let ds = List.map (distance pos) [p1;p2;p3;p4] in
  let dist = List.fold_left min max_float ds in
  dist < width /. 2.0 

(** [corner_collide st pos width] is [true] if [pos] with [width] collides with
    a corner of the maze.*)
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

(** [count_ball_owners player balls] is the number of balls in play 
    owned by [player]*)
let count_ball_owners player balls =
  let f = match player with 
    | One -> fun a x -> if x.owner.player_num = One then 1 else 0
    | Two -> fun a x -> if x.owner.player_num = Two then 1 else 0
  in List.fold_left f 0 balls

(** [remove_balls st] is [st] with all expired balls removed from the [ball_list]*)
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

(** [is_collision ball camel] is [true] if [ball] is currently in [camel]*)
let is_collision ball camel =
  distance camel.pos ball.position <= camel_width/.2.0 +. ball_width/.2.0

(** [kill st player_num] is [st] with [camel1alive] = false if [player_num] is 
    One and [camel2alive] = false if [player_num] is Two*)
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

(** [handle_death_collision ball st] is the state after handling collisions
    in [move_ball] *)
let handle_death_collision ball st =
  if is_collision ball st.camel1
  then kill st One |> reinit
  else if is_collision ball st.camel2
  then kill st Two |> reinit
  else st

(** [handle_horiz_ball b] is the ball [b] after a horizontal collision*)
let handle_horiz_ball b = 
  let b' = Ball.flip_h b in
  let x' = Ball.new_pos_x b' in
  let p = Position.init x' b'.position.y in
  {b' with position = p} |> step_timer

(** [handle_vert_ball b] is the ball [b] after a vertical collision*)
let handle_vert_ball b = 
  let b' = Ball.flip_v b  in
  let y' = Ball.new_pos_y b' in
  let p = Position.init b'.position.x y' in
  {b' with position = p} |> step_timer

(** [handle_corner_ball b] is the ball [b] after a corner collision*)
let handle_corner_ball b =
  if (current_square Y b.position) = (current_square Y b.position |> truncate) 
  then
    handle_horiz_ball b
  else
    handle_vert_ball b

(** [move_ball st b] is [b] with a new position and angle of movement after 
    moving by one unit. *)
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

(** [horiz_pos camel speed] is the [camel] with new horz positions after moving*)
let horiz_pos camel speed = 
  {camel.pos with x = Camel.move_horiz camel.pos.x camel.dir speed}

(** [vert_pos camel speed] is the [camel] with new vert positions after moving*)
let vert_pos camel speed = 
  {camel.pos with y = Camel.move_vert camel.pos.y camel.dir speed}

(** [any_collision st pos width] is [true] if there are any collisions*)
let any_collision st pos width =
  corner_collide st pos width 
  || horiz_collide st pos width
  || vert_collide st pos width

(** [translated_pos st default_pos h_pos v_pos width] is pos after collision*)
let translated_pos st default_pos h_pos v_pos width = 
  match any_collision st h_pos width, any_collision st v_pos width with 
  | true, true -> default_pos
  | true, false -> v_pos 
  | false, true | false, false -> h_pos

(** [after_corner st prev_pos next_pos width] is the pos after corner collide*)
let after_corner st prev_pos next_pos width = 
  if corner_collide st prev_pos camel_width
  then next_pos
  else prev_pos

(** [move_camel st camel speed] is new position of [camel] after a move action
    at a specific [speed] *)
let move_camel st camel speed =
  let new_pos = Position.init 
      (Camel.move_horiz camel.pos.x camel.dir speed)
      (Camel.move_vert camel.pos.y camel.dir speed) in
  let h_pos = horiz_pos camel speed in 
  let v_pos = vert_pos camel speed in
  let t_pos = translated_pos st new_pos h_pos v_pos camel_width in
  let c_pos = after_corner st new_pos t_pos camel_width in
  let h_col = horiz_collide st c_pos camel_width in 
  let v_col = vert_collide st c_pos camel_width in
  match h_col, v_col with 
  | true, true -> camel.pos
  | true, false -> h_pos
  | false, true -> v_pos
  | false, false -> c_pos

(** [move_fwd_collide st camel] is the new [camel] after moving forward*)
let move_fwd_collide st camel = 
  {camel with pos = move_camel st camel Camel.fwd_speed}

(** [move_rev_collide st camel] is the new [camel] after moving behind*)
let move_rev_collide st camel = 
  {camel with pos = move_camel st camel Camel.rev_speed}

(** [ball_shot_xy camel] is the new pos of [camel] after *)
let ball_shot_xy camel = 
  let total_width = ball_width /. 4.0 +. camel_width /. 2.0 in
  let x = camel.pos.x +. (total_width *. cosine (90.0 -. camel.dir)) in
  let y = camel.pos.y -. (total_width *. sine (90.0 -. camel.dir)) in
  x, y

(** [handle_shot camel st] is the [st] after a [camel] is shot*)
let handle_shot camel st = 
  shoot_sound ();
  let x, y = ball_shot_xy camel in
  if any_collision st (Position.init x y) ball_width then
    kill st camel.player_num |> reinit
  else
    let ball = Ball.init camel camel.dir x y in
    let camel' = {camel with num_balls=camel.num_balls+1} in
    let st' = {st with ball_list=ball::st.ball_list} in
    match camel.player_num with
    | One -> {st' with camel1=camel'}
    | Two -> {st' with camel2=camel'}

let shoot camel st = 
  let curr_time = Unix.gettimeofday () in 
  if curr_time -. camel.shot_time < 0.25 then st
  else begin 
    let camel' = {camel with shot_time = curr_time} in
    if camel'.num_balls >= 5 
    then st 
    else handle_shot camel' st
  end 

let move direction st camel =
  let camel' = match direction with
    | Forward -> move_fwd_collide st camel
    | Reverse -> move_rev_collide st camel in
  match camel.player_num with
  | One -> {st with camel1 = camel'}
  | Two -> {st with camel2 = camel'}

let rotate d st camel =
  let camel' = match d with
    | CounterClockwise -> Camel.turn_left camel
    | Clockwise -> Camel.turn_right camel in
  match camel.player_num with 
  | One -> {st with camel1 = camel'}
  | Two -> {st with camel2 = camel'}

(** [check_death st balls] is the new [st] after checking if any [balls] collide 
    with camels *)
let check_death st =
  let rec check_death' st aux_balls all_balls =
    match aux_balls with
    | [] -> {st with ball_list = all_balls}
    | ball::t -> 
      if is_collision ball st.camel1 || is_collision ball st.camel2
      then handle_death_collision ball st
      else check_death' st t all_balls
  in check_death' st st.ball_list st.ball_list

(** [move_all_balls st] is the new [st] after all balls have been moved *)
let move_all_balls st =
  let balls = 
    List.fold_left (fun a x -> (move_ball st x)::a) [] st.ball_list in
  {st with ball_list = balls}
  |> remove_balls
  |> check_death

let rot_point x y center_x center_y angle =
  let x' = x -. center_x in
  let y' = y -. center_y in
  let rot_x = x'*.(cosine angle) -. y'*.(sine angle) in
  let rot_y = x'*.(sine angle) +. y'*.(cosine angle) in
  (rot_x +. center_x |> int_of_float, rot_y +. center_y |> int_of_float)

(** [update_state state] is the new [state] after moving all balls*)
let rec update_state state =
  move_all_balls state

(** [init_camel1] is the initial state of player 1's camel*)
let init_camel1 = Camel.init One 0.0 0.0 ~-1 (Resources.get_input_name One)
(** [init_camel2] is the initial state of player 2's camel*)
let init_camel2 = Camel.init Two 0.0 0.0 ~-1 (Resources.get_input_name Two)

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
