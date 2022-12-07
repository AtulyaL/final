open OUnit2
open Game
open Pieces
open Board
open Logic
(* open Command *)

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

(* (** [update_board_test name] constructs an OUnit test in [board_tests] that
   asserts the quality of [expected_output] with [update_board move piece].
   Requires that move must be valid.*) let update_board_test (name : string)
   (board : board) (move : int * int) (piece : Pieces.t) : test = let old_loc =
   Pieces.(location piece) in let updated_board = update_board board move piece
   in name >:: fun _ -> assert_equal (is_empty updated_board old_loc) true;
   assert_equal (is_empty updated_board move) false; assert_equal (find_piece
   move updated_board) piece *)

(** [find_piece_test name] constructs an OUnit test in [board_tests] that
    asserts the quality of [expected_output] with [find_piece coord board]. *)
let find_piece_test (name : string) (coord : int * int) (board : board)
    (expected_output : Pieces.t) : test =
  name >:: fun _ -> assert_equal expected_output (find_piece coord board)

(* (** [to_string_test name] constructs an OUnit test in [board_tests] that
   asserts the quality of [expected_output] with [to_string board]. *) let
   to_string_test (name : string) (board : board) (expected_output : string) :
   test = name >:: fun _ -> assert_equal expected_output (to_string board)

   (** [to_lst_test name] constructs an OUnit test in [board_tests] that asserts
   the quality of [expected_output] with [to_list board]. *) let to_lst_test
   (name : string) (board : board) (expected_output : string list list) : test =
   name >:: fun _ -> assert_equal expected_output (to_lst board)

   (** [is_empty_test name] constructs an OUnit test in [board_tests] that
   asserts the quality of [expected_output] with [is_empty board coord]. *) let
   is_empty_test (name : string) (board : board) (coord : int * int)
   (expected_output : bool) : test = name >:: fun _ -> assert_equal
   expected_output (is_empty board coord)

   (** [isolate_black_test name] constructs an OUnit test in [board_tests] that
   asserts the quality of [expected_output] with [isolate_black board]. *) let
   isolate_black_test (name : string) (board : board) (expected_output : board)
   : test = name >:: fun _ -> assert_equal expected_output (isolate_black board)

   (** [isolate_white_test name] constructs an OUnit test in [board_tests] that
   asserts the quality of [expected_output] with [isolate_white board]. *) let
   isolate_white_test (name : string) (board : board) (expected_output : board)
   : test = name >:: fun _ -> assert_equal expected_output (isolate_white
   board) *)

(*****************************************************************)
(* Helper Functions for Command *)
(*****************************************************************)

(*****************************************************************)
(* Helper Functions for Logic *)
let check_mate_two_rooks : board =
  let res = ref [] in
  empty_board res;
  let k1 = update_board !res (1, 4) (Pieces.init King White (1, 4)) in
  let r1 = update_board k1 (2, 1) (Pieces.init Rook Black (2, 1)) in
  let k2 = update_board r1 (8, 1) (Pieces.init King Black (8, 1)) in
  update_board k2 (1, 1) (Pieces.init Rook Black (1, 1))

(*****************************************************************)

(* * [check_move_test name] constructs an OUnit test in [logic_tests] that
   asserts the quality of [expected_output] with [check_move move info board
   color]. *)
let check_move_test (name : string) (move : int * int) (info : Pieces.t)
    (board : board) (color : Pieces.color) (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (check_move move info board color)

(** [check_test name] constructs an OUnit test in [logic_tests] that asserts the
    quality of [expected_output] with [check board color]. *)
(* let check_test (name : string) (board : board) (color : Pieces.color)
   (expected_output : bool) : test = name >:: fun _ -> assert_equal
   expected_output (check board color) *)

(** [check_mate_test name] constructs an OUnit test in [logic_tests] that
    asserts the quality of [expected_output] with [check_mate board color]. *)
let check_mate_test (name : string) (board : board) (color : Pieces.color)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (check_mate board color)

(* (** [update_status_test name] constructs an OUnit test in [logic_tests] that
   asserts the quality of [expected_output] with [update_status status]. *) let
   update_status_test (name : string) (status : status) (expected_output :
   status) : test = name >:: fun _ -> assert_equal expected_output
   (update_status status) *)

(*****************************************************************)
(* Helper Functions for Pieces *)
(*****************************************************************)
let rec pawn_army_check color =
  let res = ref [] in
  match color with
  | White ->
      loop_through_row 2 White res;
      !res
  | Black ->
      loop_through_row 7 Black res;
      !res

and loop_through_row row color tests =
  let i = ref 1 in
  let name =
    color_to_string color ^ " pawn should be at " ^ "(" ^ string_of_int row
    ^ "," ^ string_of_int !i ^ ")"
  in
  while !i < 9 do
    tests :=
      find_piece_test name (row, !i) init (Pieces.init Pawn color (row, !i))
      :: !tests;
    i := !i + 1
  done

let pawn_init_check = pawn_army_check White @ pawn_army_check Black

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
    (*Testing initalization of pieces besides pawn is correct*)
    (*White side*)
    find_piece_test "White rook 1 should be at (1,1)" (1, 1) init
      (Pieces.init Rook White (1, 1));
    find_piece_test "White knight 1 should be at (1,2)" (1, 2) init
      (Pieces.init Knight White (1, 2));
    find_piece_test "White bishop 1 should be at (1,3)" (1, 3) init
      (Pieces.init Bishop White (1, 3));
    find_piece_test "White Queen should be at (1,4)" (1, 4) init
      (Pieces.init Queen White (1, 4));
    find_piece_test "White King should be at (1,5)" (1, 5) init
      (Pieces.init King White (1, 5));
    find_piece_test "White bishop 2 should be at (1,6)" (1, 6) init
      (Pieces.init Bishop White (1, 6));
    find_piece_test "White knight 2 should be at (1,7)" (1, 7) init
      (Pieces.init Knight White (1, 7));
    find_piece_test "White rook 2 should be at (1,1)" (1, 8) init
      (Pieces.init Rook White (1, 8));
    (*Black side*)
    find_piece_test "Black rook should be at (8,1)" (8, 1) init
      (Pieces.init Rook Black (8, 1));
    find_piece_test "Black knight 1 should be at (8,2) " (8, 2) init
      (Pieces.init Knight Black (8, 2));
    find_piece_test "Black bishop 1 should be at (8,3) " (8, 3) init
      (Pieces.init Bishop Black (8, 3));
    find_piece_test "Black Queen should be at (8,4) " (8, 4) init
      (Pieces.init Queen Black (8, 4));
    find_piece_test "Black King should be at (8,5) " (8, 5) init
      (Pieces.init King Black (8, 5));
    find_piece_test "Black bishop 2 should be at (8,6) " (8, 6) init
      (Pieces.init Bishop Black (8, 6));
    find_piece_test "Black knight 2 should be at (8,7) " (8, 7) init
      (Pieces.init Knight Black (8, 7));
    find_piece_test "Black rook 2 should be at (8,8) " (8, 8) init
      (Pieces.init Rook Black (8, 8))
    (*-------------------------------------------------------*);
  ]
  (*Check pawns are initialized at the right places*)
  @ pawn_init_check

let command_tests = []

let logic_tests =
  [
    check_move_test "white pawn cannot teleport to where rook is" (1, 1)
      (Pieces.init Pawn White (2, 1))
      init White false;
    check_move_test "black cannot move white pawn" (2, 2)
      (Pieces.init Pawn White (2, 1))
      init Black false;
    check_move_test "white pawn cannot move in place" (2, 1)
      (Pieces.init Pawn White (2, 1))
      init White false;
    check_move_test "white pawn can move 1 forward initially" (3, 1)
      (Pieces.init Pawn White (2, 1))
      init White true;
    check_move_test "white pawn can move 2 forward initially" (4, 1)
      (Pieces.init Pawn White (2, 1))
      init White true;
    check_move_test "white pawn cannot move 2 after moving already" (6, 1)
      (Pieces.init Pawn White (3, 1))
      init White false;
    check_move_test "white pawn cannot move diagonally if empty" (2, 2)
      (Pieces.init Pawn White (2, 1))
      init White false;
    (let capture_poss =
       update_board init (3, 3) (Pieces.init Pawn Black (7, 2))
     in
     check_move_test
       "white pawn can move diagonally right to capture an enemy piece" (3, 3)
       (Pieces.init Pawn White (2, 2))
       capture_poss White true);
    (let capture_poss =
       update_board init (3, 3) (Pieces.init Pawn White (7, 2))
     in
     check_move_test
       "white pawn cannot move diagonally to capture an ally piece" (3, 3)
       (Pieces.init Pawn White (2, 2))
       capture_poss White false);
    (let capture_poss =
       update_board init (3, 1) (Pieces.init Pawn Black (7, 2))
     in
     check_move_test
       "white pawn can move left diagonally to capture an enemy piece" (3, 1)
       (Pieces.init Pawn White (2, 2))
       capture_poss White true);
    check_move_test "white cannot move a black piece" (6, 1)
      (Pieces.init Pawn Black (7, 1))
      init White false;
    (let capture_poss =
       update_board init (6, 1) (Pieces.init Pawn White (2, 1))
     in
     check_move_test
       "black pawn can move left diagonally to capture an enemy piece" (6, 1)
       (Pieces.init Pawn Black (7, 2))
       capture_poss Black true);
    check_move_test "black pawn can move forward 2 spaces initally" (5, 1)
      (Pieces.init Pawn Black (7, 1))
      init Black true;
    check_move_test "black pawn cannot move 2 after moving already" (5, 1)
      (Pieces.init Pawn Black (3, 1))
      init Black false;
    check_move_test "black pawn can move forward 1" (6, 1)
      (Pieces.init Pawn Black (7, 1))
      init Black true;
    check_move_test "white knight can make a legal move from start" (3, 6)
      (Pieces.init Knight White (1, 7))
      init White true;
    check_move_test "white knight cannot make an illegal move from start" (4, 6)
      (Pieces.init Knight White (1, 7))
      init White false;
    check_move_test "right white knight cannot move to occupied from start"
      (2, 5)
      (Pieces.init Knight White (1, 7))
      init White false;
    check_move_test "left white knight cannot move to occupied from start" (2, 4)
      (Pieces.init Knight White (1, 2))
      init White false;
    (let board_w_knight_in_middle =
       update_board init (5, 4) (Pieces.init Knight White (1, 2))
     in
     check_move_test
       "left white knight in middle of board can move backwards in side L to \
        left"
       (4, 2)
       (Pieces.init Knight White (5, 4))
       board_w_knight_in_middle White true);
    (let board_w_knight_in_middle =
       update_board init (5, 4) (Pieces.init Knight White (1, 2))
     in
     check_move_test
       "left white knight in middle of board can move backwards in upright L \
        to left"
       (3, 3)
       (Pieces.init Knight White (5, 4))
       board_w_knight_in_middle White true);
    (let board_w_knight_in_middle =
       update_board init (5, 4) (Pieces.init Knight White (1, 2))
     in
     check_move_test
       "left white knight in middle of board can move backwards in side L to \
        right"
       (4, 6)
       (Pieces.init Knight White (5, 4))
       board_w_knight_in_middle White true);
    (let board_w_knight_in_middle =
       update_board init (5, 4) (Pieces.init Knight White (1, 2))
     in
     check_move_test
       "left white knight in middle of board can move backwards in upright L \
        to right"
       (3, 5)
       (Pieces.init Knight White (5, 4))
       board_w_knight_in_middle White true);
    check_move_test "knight cannot move straight" (3, 2)
      (Pieces.init Knight White (1, 2))
      init White false;
    (let board_w_knight_in_middle =
       update_board init (5, 4) (Pieces.init Knight White (1, 2))
     in
     check_move_test
       "left white knight in middle of board canot move backwards in side L to \
        left that is too large"
       (3, 2)
       (Pieces.init Knight White (5, 4))
       board_w_knight_in_middle White false);
    check_move_test "rook cannot hop over pieces" (5, 1)
      (Pieces.init Rook White (1, 1))
      init White false;
    (let free_rook = update_board init (2, 1) (Pieces.init Rook White (1, 1)) in
     check_move_test "rook can move straight forward as long as free" (5, 1)
       (Pieces.init Rook White (2, 1))
       free_rook White true);
    (let free_rook = update_board init (3, 1) (Pieces.init Rook White (1, 1)) in
     check_move_test "rook can move straight side as long as free" (3, 5)
       (Pieces.init Rook White (3, 1))
       free_rook White true);
    (let middle_rook =
       update_board init (4, 4) (Pieces.init Rook White (1, 1))
     in
     check_move_test "rook can move straight left as long as free" (4, 1)
       (Pieces.init Rook White (4, 4))
       middle_rook White true);
    (let middle_rook =
       update_board init (4, 4) (Pieces.init Rook White (1, 1))
     in
     check_move_test "rook can move straight down 1 as long as free" (3, 4)
       (Pieces.init Rook White (4, 4))
       middle_rook White true);
    (let obstr = update_board init (4, 4) (Pieces.init Rook White (1, 1)) in
     let middle_rook_obstructed =
       update_board obstr (4, 2) (Pieces.init Pawn White (2, 2))
     in
     check_move_test "rook can move straight left as long as free" (4, 1)
       (Pieces.init Rook White (4, 4))
       middle_rook_obstructed White false);
    (let obstr = update_board init (5, 4) (Pieces.init Rook White (1, 1)) in
     let middle_rook_obstructed =
       update_board obstr (4, 4) (Pieces.init Pawn White (2, 4))
     in
     check_move_test "rook can move straight left as long as free" (3, 4)
       (Pieces.init Rook White (5, 4))
       middle_rook_obstructed White false);
    (let free_down = update_board init (5, 4) (Pieces.init Rook White (1, 1)) in
     check_move_test "rook can move straight down many steps as long as free"
       (3, 4)
       (Pieces.init Rook White (5, 4))
       free_down White true);
    (let free_rook = update_board init (5, 1) (Pieces.init Rook White (1, 1)) in
     let obstr =
       update_board free_rook (5, 3) (Pieces.init Pawn White (2, 3))
     in
     check_move_test "rook can move straight right as long as free" (5, 5)
       (Pieces.init Rook White (5, 1))
       obstr White false);
    check_move_test "rook can only move straight" (2, 2)
      (Pieces.init Rook White (5, 1))
      init White false;
    check_move_test "bishop can only move diagonal" (1, 1)
      (Pieces.init Bishop White (1, 3))
      init White false;
    (let free_bishop =
       update_board init (5, 4) (Pieces.init Bishop White (1, 3))
     in
     check_move_test "bishop can move diagonally NE" (6, 5)
       (Pieces.init Bishop White (5, 4))
       free_bishop White true);
    (let free_bishop =
       update_board init (5, 4) (Pieces.init Bishop White (1, 3))
     in
     check_move_test "bishop can move diagonally NE until blocked" (8, 6)
       (Pieces.init Bishop White (5, 4))
       free_bishop White false);
    (let free_bishop =
       update_board init (5, 4) (Pieces.init Bishop White (1, 3))
     in
     check_move_test "bishop can move diagonally SE until blocked" (2, 1)
       (Pieces.init Bishop White (5, 4))
       free_bishop White false);
    (let free_bishop =
       update_board init (5, 4) (Pieces.init Bishop White (1, 3))
     in
     check_move_test "bishop can move diagonally SE " (3, 2)
       (Pieces.init Bishop White (5, 4))
       free_bishop White true);
    (let free_bishop =
       update_board init (5, 4) (Pieces.init Bishop White (1, 3))
     in
     check_move_test "bishop can move diagonally NW " (6, 3)
       (Pieces.init Bishop White (5, 4))
       free_bishop White true);
    (let free_bishop =
       update_board init (5, 4) (Pieces.init Bishop White (1, 3))
     in
     check_move_test "bishop can move diagonally NW until it's blocked" (8, 1)
       (Pieces.init Bishop White (5, 4))
       free_bishop White false);
    (let free_bishop =
       update_board init (5, 4) (Pieces.init Bishop White (1, 3))
     in
     check_move_test "bishop can move diagonally SW " (3, 2)
       (Pieces.init Bishop White (5, 4))
       free_bishop White true);
    check_move_test "king cant move 2 forward" (3, 5)
      (Pieces.init King White (1, 5))
      init White false;
    check_move_test "king cant move 2 diagonal right" (3, 6)
      (Pieces.init King White (1, 5))
      init White false;
    check_move_test "king cant move 2 left" (1, 1)
      (Pieces.init King White (1, 5))
      init White false;
    check_move_test "king cant move 2 left" (1, 6)
      (Pieces.init King White (1, 5))
      init White false;
    check_move_test "king cant move 2 diagonal left" (3, 6)
      (Pieces.init King White (1, 5))
      init White false;
    check_move_test "king cant teleport to a random place" (5, 1)
      (Pieces.init King White (1, 4))
      init White false;
    (let free_king = update_board init (5, 4) (Pieces.init King White (1, 5)) in
     check_move_test "king can move 1 forward" (6, 4)
       (Pieces.init King White (5, 4))
       free_king White true);
    (let free_king = update_board init (5, 4) (Pieces.init King White (1, 5)) in
     check_move_test "king can move 1 backward" (4, 4)
       (Pieces.init King White (5, 4))
       free_king White true);
    (let free_king = update_board init (5, 4) (Pieces.init King White (1, 5)) in
     check_move_test "king can move 1 right" (5, 5)
       (Pieces.init King White (5, 4))
       free_king White true);
    (let free_king = update_board init (5, 4) (Pieces.init King White (1, 5)) in
     check_move_test "king can move 1 left" (5, 3)
       (Pieces.init King White (5, 4))
       free_king White true);
    (let free_king = update_board init (5, 4) (Pieces.init King White (1, 5)) in
     check_move_test "king can move 1 diagonal top right" (6, 5)
       (Pieces.init King White (5, 4))
       free_king White true);
    (let free_king = update_board init (5, 4) (Pieces.init King White (1, 5)) in
     check_move_test "king can move 1 diagonal top left" (6, 3)
       (Pieces.init King White (5, 4))
       free_king White true);
    (let free_king = update_board init (5, 4) (Pieces.init King White (1, 5)) in
     check_move_test "king can move 1 diagonal bottom left" (4, 3)
       (Pieces.init King White (5, 4))
       free_king White true);
    (let free_king = update_board init (5, 4) (Pieces.init King White (1, 5)) in
     check_move_test "king can move 1 diagonal bottom right" (4, 5)
       (Pieces.init King White (5, 4))
       free_king White true);
    check_move_test "queen moving unaturally" (3, 5)
      (Pieces.init Queen White (1, 4))
      init White false;
    (let free_queen =
       update_board init (4, 4) (Pieces.init Queen White (1, 4))
     in
     check_move_test "Queen moving diagonally bottom left" (3, 3)
       (Pieces.init Queen White (4, 4))
       free_queen White true);
    (let free_queen =
       update_board init (4, 4) (Pieces.init Queen White (1, 4))
     in
     check_move_test "Queen moving diagonally bottom right" (3, 5)
       (Pieces.init Queen White (4, 4))
       free_queen White true);
    (let free_queen =
       update_board init (4, 4) (Pieces.init Queen White (1, 4))
     in
     check_move_test "Queen moving forwards " (5, 4)
       (Pieces.init Queen White (4, 4))
       free_queen White true);
    (let free_queen =
       update_board init (4, 4) (Pieces.init Queen White (1, 4))
     in
     check_move_test "Queen moving backwards " (3, 4)
       (Pieces.init Queen White (4, 4))
       free_queen White true);
    (let free_queen =
       update_board init (4, 4) (Pieces.init Queen White (1, 4))
     in
     check_move_test "Queen moving right " (4, 5)
       (Pieces.init Queen White (4, 4))
       free_queen White true);
    check_mate_test "checkmate with two rooks" check_mate_two_rooks White true;
  ]

let pieces_tests = []

let suite =
  "test suite for Chess Game"
  >::: List.flatten [ board_tests; command_tests; logic_tests; pieces_tests ]

let _ = run_test_tt_main suite