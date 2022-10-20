open OUnit2
open Game
open Pieces
open Board
open Logic

(** [init name] constructs an OUnit test in [board_tests] that asserts the
    quality of [expected_output] with [init]. *)
let init_test (name : string) (expected_output : board) : test =
  name >:: fun _ -> assert_equal expected_output init

let board_tests = []
let logic_tests = []
let pieces_tests = []

let suite =
  "test suite for Chess Game"
  >::: List.flatten [ board_tests; logic_tests; pieces_tests ]

let _ = run_test_tt_main suite