open OUnit2
open Maze

let make_test_temp 
    (name : string)
    (filename : string) 
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output ((fun d -> true) "") )

(* CAMEL TEST CASES *)
let test_move_horiz 
    (name : string)
    (x : float)
    (dir : float)
    (speed : float) 
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Camel.move_horiz x dir speed |> int_of_float) )

let test_move_vert 
    (name : string)
    (y : float)
    (dir : float)
    (speed : float) 
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Camel.move_vert y dir speed |> int_of_float) )

let camel1 : Camel.t =  {
  player_num = One;
  score = 0;
  num_balls = 0;
  dir = 0.0; (* degrees 0 - 360 *)
  pos = Position.init 0.0 0.0;
  shot_time = 0.0; 
}
let camel2 = Camel.turn_right camel1 
let camel3 = Camel.turn_right camel2 

let camel2l = Camel.turn_left camel1 
let camel3l = Camel.turn_left camel2l

let get_dir (cam:Camel.t) = cam.dir 

let test_turn (*Compares the directions that have been modified*)
    (name : string)
    (camel : Camel.t)
    (f : Camel.t -> Camel.t)
    (expected_output : float) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (f camel |> get_dir))

let get_pn (camel: Camel.t) = camel.player_num

let test_init_pn 
    (name : string)
    (pnum : Camel.player_num)
    (expected_output : Camel.player_num) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (( Camel.init pnum 0. 0. 0) |> get_pn) )

let get_pos (camel: Camel.t) = camel.pos 

let test_init_pos 
    (name : string)
    (pos : Position.t)
    (expected_output : Position.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output ((Camel.init One pos.x pos.y 0) |> get_pos))


let camel_tests = [
  test_move_horiz "Don't move - speed 0" 0. 1. 0. 0;
  test_move_horiz "Move - speed 1"  1. 1. 1. 1; 
  test_move_horiz "Move faster - speed 50"  1. 10. 50. 9; 
  test_move_vert "Don't move - speed 0" 0. 1. 0. 0;
  test_move_vert "Move - speed 5"  1. 5. 5. (-3); 
  test_move_vert "Move - speed 5"  1. 2. 7. (-5);
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
    the quality of [expected_output] with angle
    of [f ball] *)
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

let ball_tests = [
  test_ball_pos "move initial ball pos" ball0 (Position.init 0. (-5.));
  test_ball_pos "move ball pos w angle" ball1 (Position.init 2. (-10.));
  test_ball_pos "move ball pos w angle" ball2 (Position.init (-3.) (-10.));

  test_ball_flip "flip horiz initial ball" ball0 Ball.flip_h 180.;
  test_ball_flip "flip horiz ball w angle" ball1 Ball.flip_h 150.;
  test_ball_flip "flip horiz ball w large angle" ball3 Ball.flip_h 350.;
  test_ball_flip "flip vert initial ball" ball0 Ball.flip_v 360.;
  test_ball_flip "flip vert ball w angle" ball1 Ball.flip_v 330.;

  test_ball_timer "decrement full timer" ball0 19.9;

]

(** [test_pos_dist name pos1 pos2 expected_output] asserts
    the quality of [expected_output] with [Position.distance pos1 pos2] *)
let test_pos_dist
    (name : string)
    (pos1 : Position.t)
    (pos2 : Position.t)
    (expected_output : float) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Position.distance pos1 pos2) )

let position_tests = [
  test_pos_dist "euclidean distance between (0,0) and (0,0)"
    (Position.init 0. 0.) (Position.init 0. 0.) 0.;
  test_pos_dist "euclidean distance between (0,0) and (0,2)"
    (Position.init 0. 0.) (Position.init 0. 2.) 2.;
  test_pos_dist "euclidean distance between (0,0) and (3,4)"
    (Position.init 0. 0.) (Position.init 3. 4.) 5.;
]

(** [test_util_trig name degree f expected_output] asserts
    the quality of [expected_output] with [f degree] *)
let test_util_trig 
    (name : string)
    (degree : float)
    (f : float -> float)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (f degree |> int_of_float) )

let util_tests = [
  test_util_trig "Test cosine degree function with PI/2"
    90. Utils.cosine (Stdlib.cos (Float.pi /. 2.) |> int_of_float);
  test_util_trig "Test cosine degree function with PI"
    180. Utils.cosine (Stdlib.cos (Float.pi) |> int_of_float);
  test_util_trig "Test sine degree function with PI/2"
    90. Utils.sine (Stdlib.sin (Float.pi /. 2.) |> int_of_float);
  test_util_trig "Test sine degree function with PI"
    180. Utils.sine (Stdlib.sin (Float.pi) |> int_of_float);
]

let suite = "search test suite" >::: List.flatten [
    camel_tests;
    ball_tests;
    position_tests;
    util_tests
  ]

let _ = run_test_tt_main suite