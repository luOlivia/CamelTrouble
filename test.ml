(* TEST PLAN 
   In testing CamelTrouble, we invoked both OUnit tests and play testing.
   We automatically tested the modules Camel, Ball, Position, Util, and Maze as
   they build the core components of our game. Specifically, we tested that 
   movement functions correctly calculated new positions based on current 
   position and object angle. We also tested that our helper functions to 
   calculate cosine, sine angles, and truncation produced the correct output.
   For maze, we verified that certain walls exist based on a Maze object. 

   Test cases for ball movement was developed through glass-box testing in 
   order to cover the different angle cases to be path complete. The same 
   was done for camel movement. For Maze, we used black-box testing because 
   maze generation is inherently random, so we checked that the maze could 
   actually be generated and not enter an infinite loop. We also created a 
   maze example to test wall placements with black-box testing.  

   Main, state, and resources were play-tested for accuracy due to the 
   constantly changing state. We tested that each camel could move via 
   keyboard input, camels and balls cannot move through walls, and camels 
   die on collision with balls. Since these features are central to the 
   gameplay, we can manually verify that these events occur and our system 
   is correct. The resources module handles the graphical drawings on our gui,
    so we could verify that they were displaying correctly manually. 

   Overall, gameplay proceeds as expected. Therefore in conjunction with our 
   OUnit tests, we can demonstrate the correctness of our system.
*)

open OUnit2
open Maze

(* CAMEL TEST CASES *)

(** [test_move name x dir speed f expected_output] asserts
    the quality of [expected_output] with [f x dir speed] *)
let test_move 
    (name : string)
    (x : float)
    (dir : float)
    (speed : float) 
    (f : float -> float -> float -> float)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (f x dir speed |> int_of_float))

(** [camel1] is initial camel *)
let camel1 : Camel.t =  {
  player_num = One;
  score = 0;
  num_balls = 0;
  dir = 0.0; (* degrees 0 - 360 *)
  pos = Position.init 0.0 0.0;
  shot_time = 0.0; 
  player_name = ""
}
(** [camel2] is 90 degree right camel *)
let camel2 = Camel.turn_right camel1 
(** [camel3] is 180 degree right camel *)
let camel3 = Camel.turn_right camel2 
(** [camel2l] is 90 degree left camel *)
let camel2l = Camel.turn_left camel1 
(** [camel3l] is 180 degree left camel *)
let camel3l = Camel.turn_left camel2l

(** [test_turn name camel f expected_output] asserts
    the quality of [expected_output] with direction of [f camel] *)
let test_turn
    (name : string)
    (camel : Camel.t)
    (f : Camel.t -> Camel.t)
    (expected_output : float) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (f camel).dir)

(** [test_init_pn name pnum expected_output] asserts
    the quality of [expected_output] with new camel's [pnum] *)
let test_init_pn 
    (name : string)
    (pnum : Camel.player_num)
    (expected_output : Camel.player_num) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Camel.init pnum 0. 0. 0 "").player_num)

(** [test_init_pos name pos expected_output] asserts
    the quality of [expected_output] with new camel's [pos] *)
let test_init_pos 
    (name : string)
    (pos : Position.t)
    (expected_output : Position.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Camel.init One pos.x pos.y 0 "").pos)

(** [test_camel_str name camel expected_output] asserts
    the quality of [expected_output] with [Camel.to_str camel] *)
let test_camel_str 
    (name : string)
    (camel : Camel.t)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Camel.to_str camel))

(** [camel_tests] is test cases for camel module *)
let camel_tests = [
  test_move "Don't move - speed 0" 0. 1. 0. Camel.move_horiz 0;
  test_move "Move - speed 1"  1. 1. 1. Camel.move_horiz 1; 
  test_move "Move faster - speed 50"  1. 10. 50. Camel.move_horiz 9; 
  test_move "Don't move - speed 0" 0. 1. 0. Camel.move_vert 0;
  test_move "Move - speed 5"  1. 5. 5. Camel.move_vert (-3); 
  test_move "Move - speed 5"  1. 2. 7. Camel.move_vert (-5);
  test_turn "Turn right - dir=0" camel1 Camel.turn_right (-5.);  
  test_turn "Turn right - dir=-5" camel2 Camel.turn_right (-10.); 
  test_turn "Turn right - dir=-10" camel3 Camel.turn_right (-15.); 
  test_turn "Turn left - dir=0" camel1 Camel.turn_left (5.);  
  test_turn "Turn left - dir=5" camel2l Camel.turn_left (10.); 
  test_turn "Turn left - dir=10" camel3l Camel.turn_left (15.); 
  test_init_pn "Init player num - pn=One" 
    (One: Camel.player_num) (One: Camel.player_num);
  test_init_pn "Init player num - pn=Two" 
    (Two: Camel.player_num) (Two: Camel.player_num);
  test_init_pos "test init position (0,0)" 
    (Position.init 0.0 0.0) camel1.pos; 
  test_init_pos "test init position (6,0)" 
    (Position.init 6.0 0.0) (Position.init 6.0 0.0);
  test_camel_str "camel to string" camel1 
    "player one num_balls: 0\n angle dir: 0.\n pos: 0. 0."; 
]

(* BALL TEST CASES *)

(** [ball0] is a ball with no angle *)
let ball0 = Ball.init camel1 0. 0. 0.
(** [pos0] is new pos after moving ball *)
let pos0 = Position.init (Ball.new_pos_x ball0) (Ball.new_pos_y ball0)
(** [ball1] is ball with pos angle *)
let ball1 = {ball0 with angle = 30.; position = pos0}
(** [ball2] is ball with neg angle *)
let ball2 = {ball0 with angle = (-30.); position = pos0}
(** [ball3] is ball with large angle *)
let ball3 = {ball0 with angle = 190.; position = pos0}
(** [ball4] is ball with empty timer *)
let ball4 = {ball0 with timer = 0.}
(** [ball5] is ball with full angle *)
let ball5 = {ball0 with angle = 360.; position = pos0}

(** [test_ball_pos name ball expected_output] asserts
    the quality of [expected_output] with position 
    of ([Ball.new_pos_x], [Ball.new_pos_y])*)
let test_ball_pos
    (name : string)
    (ball : Ball.t)
    (expected_output : Position.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output 
        (Position.init (ball |> Ball.new_pos_x |> floor) 
           (ball |> Ball.new_pos_y |> floor)))

(** [test_ball_flip name f ball expected_output] asserts
    the quality of [expected_output] with angle of [f ball] *)
let test_ball_flip
    (name : string)
    (ball : Ball.t)
    (f : Ball.t -> Ball.t)
    (expected_output : float) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (f ball).angle)

(** [test_ball_timer name ball expected_output] asserts
    the quality of [expected_output] with [Ball.step_timer ball] *)
let test_ball_timer
    (name : string)
    (ball : Ball.t)
    (expected_output : float) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output 
        (Ball.step_timer ball).timer)

(** [ball_tests] is test cases for ball module *)
let ball_tests = [
  test_ball_pos "move initial ball pos" ball0 (Position.init 0. (-5.));
  test_ball_pos "move ball pos w angle" ball1 (Position.init 2. (-10.));
  test_ball_pos "move ball pos w angle" ball2 (Position.init (-3.) (-10.));

  test_ball_flip "flip horiz initial ball" ball0 Ball.flip_h 180.;
  test_ball_flip "flip horiz ball w angle" ball1 Ball.flip_h 150.;
  test_ball_flip "flip horiz ball w large angle" ball3 Ball.flip_h 350.;
  test_ball_flip "flip horiz ball w full angle" ball5 Ball.flip_h 180.;
  test_ball_flip "flip vert initial ball" ball0 Ball.flip_v 360.;
  test_ball_flip "flip vert ball w angle" ball1 Ball.flip_v 330.;
  test_ball_flip "flip vert ball w full angle" ball5 Ball.flip_v 0.;

  test_ball_timer "decrement full timer" ball0 19.9;
  test_ball_timer "decrement empty timer" ball4 (-0.1);
]

(* POSITION TEST CASES *)

(** [test_pos_dist name pos1 pos2 expected_output] asserts
    the quality of [expected_output] with [Position.distance pos1 pos2] *)
let test_pos_dist
    (name : string)
    (pos1 : Position.t)
    (pos2 : Position.t)
    (expected_output : float) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Position.distance pos1 pos2))

(** [position_tests] is test cases for position module *)
let position_tests = [
  test_pos_dist "euclidean distance between (0,0) and (0,0)"
    (Position.init 0. 0.) (Position.init 0. 0.) 0.;
  test_pos_dist "euclidean distance between (0,0) and (0,2)"
    (Position.init 0. 0.) (Position.init 0. 2.) 2.;
  test_pos_dist "euclidean distance between (0,0) and (3,4)"
    (Position.init 0. 0.) (Position.init 3. 4.) 5.;
]

(* UTILS TEST CASES *)

(** [test_util_trig name degree f expected_output] asserts
    the quality of [expected_output] with [f degree] *)
let test_util_trig 
    (name : string)
    (degree : float)
    (f : float -> float)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (f degree |> int_of_float) )

(** [test_util_trunc name x expected_output] asserts
    the quality of [expected_output] with [Utils.truncate x] *)
let test_util_trunc 
    (name : string)
    (x : float)
    (expected_output : float) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Utils.truncate x))

(** [util_tests] is test cases for util module *)
let util_tests = [
  test_util_trig "Test cosine degree function with PI/2"
    90. Utils.cosine (Stdlib.cos (Float.pi /. 2.) |> int_of_float);
  test_util_trig "Test cosine degree function with PI"
    180. Utils.cosine (Stdlib.cos (Float.pi) |> int_of_float);
  test_util_trig "Test sine degree function with PI/2"
    90. Utils.sine (Stdlib.sin (Float.pi /. 2.) |> int_of_float);
  test_util_trig "Test sine degree function with PI"
    180. Utils.sine (Stdlib.sin (Float.pi) |> int_of_float);
  test_util_trunc "truncate 5.2" 5.2 5.;
  test_util_trunc "truncate 5.2" (-5.2) (-5.);
]

(* MAZE TEST CASES *)

(** [check_empty walls] is whether [walls] is non-empty *)
let check_empty walls =
  match walls with 
  | Horizontal a -> (Array.length a) <> 0
  | Vertical a -> (Array.length a) <> 0

(** [test_init_maze name d expected_output] asserts
    the quality of [expected_output] with successful wall generation 
    in initializing maze w density [d] *)
let test_init_maze 
    (name : string)
    (d : int)
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      let m = Maze.make_maze d in 
      assert_equal expected_output 
        ((check_empty m.horizontal_walls) && (check_empty m.vertical_walls)))

(** [h_wall] is horizontal walls of example maze *)
let h_wall = Horizontal [| [| true; true; true |]; 
                           [| true; false; false |]; 
                           [| false; false; false |] |]
(** [v_wall] is vertical walls of example maze *)
let v_wall = Vertical [| [| true; true; true |]; 
                         [| true; false; false |]; 
                         [| false; false; false |] |]
(** [maze0] is an example maze *)
let maze0 = {horizontal_walls = h_wall; vertical_walls = v_wall}

(** [h1] is horizontal walls of example maze 1*)
let h1 = Horizontal [|[| true |]|]
(** [v1] is vertical walls of example maze 1 *)
let v1 = Vertical [|[| true |]|]
(** [maze0] is an example maze *)
let maze1 = {horizontal_walls = h1; vertical_walls = v1}

(** [test_walls name f m x y expected_output] asserts
    the quality of [expected_output] with [f m x y] *)
let test_walls 
    (name : string)
    (f : Maze.t -> int -> int -> bool)
    (m : Maze.t)
    (x : int)
    (y : int)
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (f m x y))

(** [test_camel_str name camel expected_output] asserts
    the quality of [expected_output] with [Camel.to_str camel] *)
let test_maze_str 
    (name : string)
    (m : Maze.t)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      print_endline (Maze.to_str m);
      assert_equal expected_output (Maze.to_str m))

(** [maze_tests] is test cases for maze module *)
let maze_tests = [
  test_init_maze "non-empty walls in maze" 10 true;
  test_init_maze "empty walls in maze" 0 true;

  test_walls "wall_above" Maze.is_wall_above maze0 0 0 true;
  test_walls "wall_below" Maze.is_wall_below maze0 0 0 true;
  test_walls "wall_left" Maze.is_wall_left maze0 0 0 true;
  test_walls "wall_right" Maze.is_wall_right maze0 0 0 true;
  test_walls "center no wall_above" Maze.is_wall_above maze0 1 1 false;
  test_walls "center no wall_below" Maze.is_wall_below maze0 1 1 false;
  test_walls "center no wall_left" Maze.is_wall_left maze0 1 1 false;
  test_walls "center no wall_right" Maze.is_wall_right maze0 1 1 false;
  test_walls "corner wall_above" Maze.is_wall_above maze0 2 2 false;
  test_walls "corner wall_left" Maze.is_wall_left maze0 2 2 false;

  test_walls "single cell wall_above" Maze.is_wall_above maze1 0 0 true;
  test_walls "single cell wall_left" Maze.is_wall_left maze1 0 0 true;
]

let suite = "search test suite" >::: List.flatten [
    camel_tests;
    ball_tests;
    position_tests;
    util_tests;
    maze_tests;
  ]

let _ = run_test_tt_main suite