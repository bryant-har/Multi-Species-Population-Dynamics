open Simulator
open Model
open Expression
open Eval
open Guigraph
open Gui_maker

let multigraph1 =
  init_multigraph [ 0.; 1. ] "Test Populations with an S" 0.1 [ "A"; "B" ]
    [ (0.4, 0.6, 0.7); (0.6, 0.2, 0.8) ]

let test_fun_1 =
  let test_t = ref 0. in
  fun mg ->
    test_t := !test_t +. 0.1;
    add_points_assoc
      [
        ("A", !test_t *. sin !test_t);
        ("B", 2. *. !test_t *. sin (!test_t /. 2.));
      ]
      mg

let multigraph2 =
  init_multigraph [ 0.; 1. ] "Test Populations the 2nd" 0.1 [ "A"; "B" ]
    [ (0.4, 0.9, 0.5); (0.2, 0.3, 0.2) ]

let test_fun_2 =
  let test_t = ref 0. in
  fun mg ->
    test_t := !test_t +. 0.1;
    add_points_assoc
      [
        ("A", !test_t *. !test_t *. cos !test_t);
        ("B", !test_t *. 0.05 *. !test_t *. sin !test_t);
      ]
      mg

let _ =
  init_multigraphs
    [
      (multigraph1, test_fun_1);
      (multigraph2, test_fun_2);
      (multigraph1, test_fun_1);
      (multigraph2, test_fun_2);
      (multigraph1, test_fun_1);
      (multigraph2, test_fun_2);
      (multigraph1, test_fun_1);
    ]
    500;
  launch ()
