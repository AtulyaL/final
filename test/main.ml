open OUnit2
open Game
open Pieces
open Board
open Logic

let board_tests = []
let logic_tests = []
let pieces_tests = []

let suite =
  "test suite for Chess Game"
  >::: List.flatten [ board_tests; logic_tests; pieces_tests ]

let _ = run_test_tt_main suite