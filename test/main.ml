open OUnit2
open Game
open Pieces
open Board
open Logic
open Command

(*****************************************************************)
(* Test suite *)
(*****************************************************************)

(*****************************************************************)
(* Helper Functions for Board *)
(*****************************************************************)

(** [valid_move_test name] constructs an OUnit test in [board_tests] that
    asserts the quality of [expected_output] with [valid_move board move color]. *)
let valid_move_test (name : string) (board : board) (move : int * int)
    (color : color) (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (valid_move board move color)

(** [update_board_test name] constructs an OUnit test in [board_tests] that
    asserts the quality of [expected_output] with [update_board move piece].
    Requires that move must be valid.*)
let update_board_test (name : string) (board : board) (move : int * int)
    (piece : Pieces.t) : test =
  let old_loc = Pieces.(location piece) in
  let updated_board = update_board board move piece in
  name >:: fun _ ->
  assert_equal (is_empty updated_board old_loc) true;
  assert_equal (is_empty updated_board move) false;
  assert_equal (find_piece move updated_board) piece

(** [find_piece_test name] constructs an OUnit test in [board_tests] that
    asserts the quality of [expected_output] with [find_piece coord board]. *)
let find_piece_test (name : string) (coord : int * int) (board : board)
    (expected_output : Pieces.t) : test =
  name >:: fun _ -> assert_equal expected_output (find_piece coord board)

(** [to_string_test name] constructs an OUnit test in [board_tests] that asserts
    the quality of [expected_output] with [to_string board]. *)
let to_string_test (name : string) (board : board) (expected_output : string) :
    test =
  name >:: fun _ -> assert_equal expected_output (to_string board)

(** [to_lst_test name] constructs an OUnit test in [board_tests] that asserts
    the quality of [expected_output] with [to_list board]. *)
let to_lst_test (name : string) (board : board)
    (expected_output : string list list) : test =
  name >:: fun _ -> assert_equal expected_output (to_lst board)

(** [is_empty_test name] constructs an OUnit test in [board_tests] that asserts
    the quality of [expected_output] with [is_empty board coord]. *)
let is_empty_test (name : string) (board : board) (coord : int * int)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (is_empty board coord)

(** [isolate_black_test name] constructs an OUnit test in [board_tests] that
    asserts the quality of [expected_output] with [isolate_black board]. *)
let isolate_black_test (name : string) (board : board) (expected_output : board)
    : test =
  name >:: fun _ -> assert_equal expected_output (isolate_black board)

(** [isolate_white_test name] constructs an OUnit test in [board_tests] that
    asserts the quality of [expected_output] with [isolate_white board]. *)
let isolate_white_test (name : string) (board : board) (expected_output : board)
    : test =
  name >:: fun _ -> assert_equal expected_output (isolate_white board)

(*****************************************************************)
(* Helper Functions for Command *)
(*****************************************************************)

(*****************************************************************)
(* Helper Functions for Logic *)
(*****************************************************************)

(** [check_move_test name] constructs an OUnit test in [logic_tests] that
    asserts the quality of [expected_output] with
    [check_move move info board color]. *)
let check_move_test (name : string) (move : int * int) (info : Pieces.t)
    (board : board) (color : Pieces.color) (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (check_move move info board color)

(** [check_test name] constructs an OUnit test in [logic_tests] that asserts the
    quality of [expected_output] with [check board color]. *)
let check_test (name : string) (board : board) (color : Pieces.color)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (check board color)

(** [check_mate_test name] constructs an OUnit test in [logic_tests] that
    asserts the quality of [expected_output] with [check_mate board color]. *)
let check_mate_test (name : string) (board : board) (color : Pieces.color)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (check_mate board color)

(** [update_status_test name] constructs an OUnit test in [logic_tests] that
    asserts the quality of [expected_output] with [update_status status]. *)
let update_status_test (name : string) (status : status)
    (expected_output : status) : test =
  name >:: fun _ -> assert_equal expected_output (update_status status)

(*****************************************************************)
(* Helper Functions for Pieces *)
(*****************************************************************)
let board_tests =
  [
    valid_move_test "test for if a piece can move to a place" init (1, 1) White
      false;
    valid_move_test "an empty spot should be valid to move to as black" init
      (5, 5) Black true;
    valid_move_test "moving to an unoccupied space as white should be true" init
      (5, 5) White true;
    valid_move_test "cannot move to an occupied test as black" init (8, 8) Black
      false;
  ]

let command_tests = []
let logic_tests = []
let pieces_tests = []

let suite =
  "test suite for Chess Game"
  >::: List.flatten [ board_tests; command_tests; logic_tests; pieces_tests ]

let _ = run_test_tt_main suite