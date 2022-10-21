open OUnit2
open Game
open Pieces
open Board
open Logic

(** [valid_move_test name] constructs an OUnit test in [board_tests] that
    asserts the quality of [expected_output] with [valid_move board move color]. *)
let valid_move_test (name : string) (board : board) (move : int * int)
    (color : string) (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (valid_move board move color)

let board_tests =
  [
    valid_move_test "test for if a piece can move to a place" init (1, 1)
      "white" true;
  ]

let logic_tests = []
let pieces_tests = []

let suite =
  "test suite for Chess Game"
  >::: List.flatten [ board_tests; logic_tests; pieces_tests ]

let _ = run_test_tt_main suite