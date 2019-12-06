open OUnit2
open Maze

let make_test_temp 
    (name : string)
    (filename : string) 
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output ((fun d -> true) "") )

(*START MAKING CAMEL TESTS*)
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

let camel1 = Camel.init 1 0. 0. 
let camel2 = Camel.turn_right camel1 
let camel3 = Camel.turn_right camel2 

let camel2l = Camel.turn_left camel1 
let camel3l = Camel.turn_left camel2l

let get_dir (cam:Camel.t) = cam.dir 

let test_turn_right (*Compares the directions that have been modified*)
    (name : string)
    (camel : Camel.t)
    (expected_output : float) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Camel.turn_right camel |> get_dir) )

let test_turn_left (*Compares the directions that have been modified*)
    (name : string)
    (camel : Camel.t)
    (expected_output : float) : test = 
  name >:: (fun _ -> 
      print_endline (Camel.turn_left camel |> get_dir |> string_of_float); 
      assert_equal expected_output (Camel.turn_left camel |> get_dir) )



let camel_tests = [
  test_move_horiz "Don't move - speed 0" 0. 1. 0. 0;
  test_move_horiz "Move - speed 1"  1. 1. 1. 1; 
  test_move_horiz "Move faster - speed 50"  1. 10. 50. 9; 
  test_move_vert "Don't move - speed 0" 0. 1. 0. 0;
  test_move_vert "Move - speed 5"  1. 5. 5. (-3); 
  test_move_vert "Move - speed 5"  1. 2. 7. (-5);
  test_turn_right "Turn right - dir=0" camel1 (-5.);  
  test_turn_right "Turn right - dir=-5" camel2 (-10.); 
  test_turn_right "Turn right - dir=-10" camel3 (-15.); 
  test_turn_left "Turn left - dir=0" camel1 (5.);  
  (* test_turn_right "Turn right - dir=5" camel2l (10.); 
     test_turn_right "Turn right - dir=10" camel3l (15.);  *)
]

let ball_tests = [

]

let maze_tests = [

]


let state_tests = [

]



let position_tests = [

]


let suite = "search test suite" >::: List.flatten [
    camel_tests
  ]

let _ = run_test_tt_main suite