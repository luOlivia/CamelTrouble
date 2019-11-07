open OUnit2
open Maze

let make_test_temp 
    (name : string)
    (filename : string) 
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output ((fun d -> true) "") )

let maze_tests = [

]

let suite = "search test suite" >::: List.flatten [
    maze_tests
  ]

let _ = run_test_tt_main suite