open OUnit2
open Maze

let make_test_temp 
    (name : string)
    (filename : string) 
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output ((fun d -> true) "") )

let make_print_test 
    (name : string)
    (filename : string) 
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      print_maze (maze_from_file filename) |> print_endline;
      assert_equal expected_output ((fun d -> true) "") )

let maze_tests = [
  make_print_test "Print out test maze" "maps/simple.map" true;
]

let suite = "search test suite" >::: List.flatten [
    maze_tests
  ]

let _ = run_test_tt_main suite