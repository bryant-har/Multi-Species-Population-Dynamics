(***************************************************************************
  Test plan:

  We tested all the functions in expression, eval, model, and gui.

  For functions in expression, eval, and model, we used both black box and glass
  box testing. For functions in expression, eval and model, we also used Bisect
  to reach code coverage of 100%, 93% and 97%, respectively. The only function
  in expression.ml also utilizes lexer and parser, which reached a code coverage
  of 92% and 49%, respectively. While we cannot directly test the code in parse,
  and the code coverage for the parser is not very high, we indirectly tested it
  via the user interface, where the results for our inputs met our expectations.

  For the functions about the user interface, which are in bin/main, we omitted
  OUnit testing and manually tested the functions in the user interface instead.

  For testing the gui, we created a file in ./gui/gui_test.ml that you can run
  with [make gui] which tests all the functionalities of drawing the gui at once
  on multiple multigraphs. We also created some unit tests (written with black
  box testing) in this file to test the basic functionalities of the graph
  abstract type. Finally, we manually tested the gui through [make simulate] to
  test for correct integration of the program with our gui.

  We believe that our testing approach demonstrates the correctness of the
  system， because the code coverage was over 90% for eval and model. For the
  part that was not covered, it is usually a branch that is not possible to
  occur if the implementation of some previous functions are correct. For the
  functions that we manually tested, the results for all our inputs also meet
  our expectations. In terms of the gui, we believe our testing methods
  demonstrated sufficient correctness of our program by showing correct graph
  illustrations for various outputs.
  **************************************************************************)

open OUnit2
open Simulator
open Model
open Expression
open Eval
open Guigraph
open Gui_maker

(***************************************************************************
  Tests for expression
  **************************************************************************)

(** [string_of_expr e] prints out the expression [e] in its AST form. *)
let rec string_of_expr (expr : Ast.expr) : string =
  let uop s e = s ^ string_of_expr e in
  match expr with
  | Var s -> "Var " ^ s
  | Float f -> "Float " ^ string_of_float f
  | Binop (bop, exp1, exp2) -> begin
      let f s e1 e2 =
        "Binop (" ^ s ^ ", " ^ string_of_expr e1 ^ ", " ^ string_of_expr e2
        ^ ")"
      in
      match bop with
      | Add -> f "Add" exp1 exp2
      | Mult -> f "Mult" exp1 exp2
      | Pow -> f "Pow" exp1 exp2
      | Div -> f "Div" exp1 exp2
      | LogBase -> f "LogBase" exp1 exp2
    end
  | Let (s, e1, e2) ->
      "Let (" ^ s ^ ", " ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Deriv e -> uop "Deriv " e
  | Sin e -> uop "Sin " e
  | Cos e -> uop "Cos " e
  | Tan e -> uop "Tan " e
  | Fact e -> uop "Fact " e

(** [to_expression_test expected_output input_s] constructs an OUnit test named
    [name] that asserts the equality of [expected_output] with
    [to_expression input_s]. *)
let to_expression_test (name : string) (expected_output : Ast.expr)
    (input_s : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (to_expression input_s) ~printer:string_of_expr

let parse_tests =
  [
    to_expression_test "parse string \"a + 3.141\""
      (Binop (Add, Var "a", Float 3.141))
      "a + 3.141";
    to_expression_test "parse string \"-a11 * N1 - a12 * N2\""
      (Binop
         ( Add,
           Binop (Mult, Binop (Mult, Var "a11", Var "N1"), Float ~-.1.),
           Binop (Mult, Binop (Mult, Var "a12", Var "N2"), Float ~-.1.) ))
      "-a11 * N1 - a12 * N2";
    to_expression_test "parse string \"(a + b / 2) ^ (5 - 0.01 * a)\""
      (Binop
         ( Pow,
           Binop (Add, Var "a", Binop (Div, Var "b", Float 2.)),
           Binop
             ( Add,
               Float 5.,
               Binop (Mult, Binop (Mult, Float 0.01, Var "a"), Float ~-.1.) ) ))
      "(a + b / 2) ^ (5 - 0.01 * a)";
    to_expression_test
      "parse string \"logBase 5 N1 + logBase (5 + a1) (N1 * logBase 10 N2)\""
      (Binop
         ( Add,
           Binop (LogBase, Float 5., Var "N1"),
           Binop
             ( LogBase,
               Binop (Add, Float 5., Var "a1"),
               Binop (Mult, Var "N1", Binop (LogBase, Float 10., Var "N2")) ) ))
      "logBase 5 N1 + logBase (5 + a1) (N1 * logBase 10 N2)";
    to_expression_test
      "parse string \"Sin x + Cos (a + b) + Tan (y / z) + Sin a - Cos b / Tan \
       c\""
      (Binop
         ( Add,
           Binop
             ( Add,
               Binop
                 ( Add,
                   Binop
                     (Add, Sin (Var "x"), Cos (Binop (Add, Var "a", Var "b"))),
                   Tan (Binop (Div, Var "y", Var "z")) ),
               Sin (Var "a") ),
           Binop (Mult, Binop (Div, Cos (Var "b"), Tan (Var "c")), Float ~-.1.)
         ))
      "Sin x + Cos (a + b) + Tan (y / z) + Sin a - Cos b / Tan c";
    to_expression_test "parse string \"N' + (N ^ 2 + 5 / N ^ 3)'\""
      (Binop
         ( Add,
           Deriv (Var "N"),
           Deriv
             (Binop
                ( Add,
                  Binop (Pow, Var "N", Float 2.),
                  Binop (Div, Float 5., Binop (Pow, Var "N", Float 3.)) )) ))
      "N' + (N ^ 2 + 5 / N ^ 3)'";
    to_expression_test "parse string \"(N + 4! - 5 * Sin 0)! * ((N + -2)')! \""
      (Binop
         ( Mult,
           Fact
             (Binop
                ( Add,
                  Binop (Add, Var "N", Fact (Float 4.)),
                  Binop
                    (Mult, Binop (Mult, Float 5., Sin (Float 0.)), Float ~-.1.)
                )),
           Fact
             (Deriv (Binop (Add, Var "N", Binop (Mult, Float 2., Float ~-.1.))))
         ))
      "(N + 4! - 5 * Sin 0)! * ((N + -2)')!";
    to_expression_test "parse string \"let x = 1 in x + 1\""
      (Let ("x", Float 1., Binop (Add, Var "x", Float 1.)))
      "let x = 1 in x + 1";
    to_expression_test
      "parse string \"let x = 1 in let x = 2 in let x = x + x in x + x * x\""
      (Let
         ( "x",
           Float 1.,
           Let
             ( "x",
               Float 2.,
               Let
                 ( "x",
                   Binop (Add, Var "x", Var "x"),
                   Binop (Add, Var "x", Binop (Mult, Var "x", Var "x")) ) ) ))
      "let x = 1 in let x = 2 in let x = x + x in x + x * x";
    to_expression_test
      "parse string \"let x = k / N + k in x ^ Sin x - x / (0.5 * x - -x')\""
      (Let
         ( "x",
           Binop (Add, Binop (Div, Var "k", Var "N"), Var "k"),
           Binop
             ( Add,
               Binop (Pow, Var "x", Sin (Var "x")),
               Binop
                 ( Mult,
                   Binop
                     ( Div,
                       Var "x",
                       Binop
                         ( Add,
                           Binop (Mult, Float 0.5, Var "x"),
                           Binop
                             ( Mult,
                               Binop (Mult, Deriv (Var "x"), Float ~-.1.),
                               Float ~-.1. ) ) ),
                   Float ~-.1. ) ) ))
      "let x = k / N + k in x ^ Sin x - x / (0.5 * x - -x')";
    to_expression_test "parse string \"3 + let x = 4! in logBase 3 (x + 3)\""
      (Binop
         ( Add,
           Float 3.,
           Let
             ( "x",
               Fact (Float 4.),
               Binop (LogBase, Float 3., Binop (Add, Var "x", Float 3.)) ) ))
      "3 + let x = 4! in logBase 3 (x + 3)";
    to_expression_test
      "parse string \"let pi = 4 * (1 - 1 / 3 + 1 / 5 - 1 / 7 + 1 / 9) in let \
       e = 1 + 1 / 1 + 1 / 2! + 1 / 3! + 1 / 4! + 1 / 5! in let i = (-1) ^ 0.5 \
       in e ^ (i * pi) + 1\""
      (Let
         ( "pi",
           Binop
             ( Mult,
               Float 4.,
               Binop
                 ( Add,
                   Binop
                     ( Add,
                       Binop
                         ( Add,
                           Binop
                             ( Add,
                               Float 1.,
                               Binop
                                 ( Mult,
                                   Binop (Div, Float 1., Float 3.),
                                   Float ~-.1. ) ),
                           Binop (Div, Float 1., Float 5.) ),
                       Binop (Mult, Binop (Div, Float 1., Float 7.), Float ~-.1.)
                     ),
                   Binop (Div, Float 1., Float 9.) ) ),
           Let
             ( "e",
               Binop
                 ( Add,
                   Binop
                     ( Add,
                       Binop
                         ( Add,
                           Binop
                             ( Add,
                               Binop
                                 ( Add,
                                   Float 1.,
                                   Binop (Div, Float 1., Fact (Float 1.)) ),
                               Binop (Div, Float 1., Fact (Float 2.)) ),
                           Binop (Div, Float 1., Fact (Float 3.)) ),
                       Binop (Div, Float 1., Fact (Float 4.)) ),
                   Binop (Div, Float 1., Fact (Float 5.)) ),
               Let
                 ( "i",
                   Binop (Pow, Binop (Mult, Float 1., Float ~-.1.), Float 0.5),
                   Binop
                     ( Add,
                       Binop (Pow, Var "e", Binop (Mult, Var "i", Var "pi")),
                       Float 1. ) ) ) ))
      "let pi = 4 * (1 - 1 / 3 + 1 / 5 - 1 / 7 + 1 / 9) in let e = 1 + 1 / 1! \
       + 1 / 2! + 1 / 3! + 1 / 4! + 1 / 5! in let i = (-1) ^ 0.5 in e ^ (i * \
       pi) + 1";
  ]

(***************************************************************************
  Tests for eval
  **************************************************************************)

(** [sub_test name expected_output input_s] constructs an OUnit test named
    [name] that asserts the equality of [convert expected_output] with
    [input_s |> to_expression |> convert |> sub].*)
let sub_test (name : string) (expected_output : Ast.expr) (input_s : string) :
    test =
  name >:: fun _ ->
  assert_equal (convert expected_output)
    (input_s |> to_expression |> convert |> sub)
    ~printer:(fun f -> f |> convert_back |> string_of_expr)

(** [force_sub_test name expected_output input_binding input_s] constructs an
    OUnit test named [name] that asserts the equality of
    [convert expected_output] with
    [input_s |> to_expression |> convert |> forceSub input_binding].*)
let force_sub_test (name : string) (expected_output : Ast.expr)
    (input_binding : string * expr) (input_s : string) : test =
  name >:: fun _ ->
  assert_equal (convert expected_output)
    (input_s |> to_expression |> convert |> forceSub input_binding)
    ~printer:(fun f -> f |> convert_back |> string_of_expr)

(** [full_eval_test name expected_output input_s] constructs an OUnit test named
    [name] that asserts the equality of [convert expected_output] with
    [input_s |> to_expression |> convert |> fullEval]. *)
let full_eval_test (name : string) (expected_output : Ast.expr)
    (input_s : string) : test =
  name >:: fun _ ->
  assert_equal (convert expected_output)
    (input_s |> to_expression |> convert |> fullEval)
    ~printer:(fun f -> f |> convert_back |> string_of_expr)

(**[regenerate_euler_test name expected_output input_point input_step input_bindings input_expr]
   constructs an OUnit test named [name] that asserts the equality of
   [convert expected_output] with
   [input_expr |> to_expression |> convert |> regenerateEuler input_point input_step input_bindings]. *)
let regenerate_euler_test (name : string) (expected_output : float * float)
    (input_point : float * float) (input_step : float)
    (input_bindings : (string * expr) list) (input_expr : string) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (input_expr |> to_expression |> convert
    |> regenerateEuler input_point input_step input_bindings)
    ~printer:(fun (x, y) ->
      "(" ^ string_of_float x ^ ", " ^ string_of_float y ^ ")")

let eval_tests =
  [
    sub_test "simplify let expressions in expression \"let x = 1 in x + 1\""
      (Binop (Add, Float 1., Float 1.))
      "let x = 1 in x + 1";
    sub_test
      "simplify let expressions in expression \"let x = 1 in let x = 2 + x in \
       let x = x + x in x + 1\""
      (Binop
         ( Add,
           Binop
             ( Add,
               Binop (Add, Float 2., Float 1.),
               Binop (Add, Float 2., Float 1.) ),
           Float 1. ))
      "let x = 1 in let x = 2 + x in let x = x + x in x + 1";
    sub_test
      "simplify let expressions in expression \"let x = 1 in Sin (x ^ Cos (x + \
       2 * Tan (x / 1) - 2) / x)\""
      (Sin
         (Binop
            ( Div,
              Binop
                ( Pow,
                  Float 1.,
                  Cos
                    (Binop
                       ( Add,
                         Binop
                           ( Add,
                             Float 1.,
                             Binop
                               ( Mult,
                                 Float 2.,
                                 Tan (Binop (Div, Float 1., Float 1.)) ) ),
                         Binop (Mult, Float 2., Float ~-.1.) )) ),
              Float 1. )))
      "let x = 1 in Sin (x ^ Cos (x + 2 * Tan (x / 1) - 2) / x)";
    sub_test
      "simplify let expressions in expression \"let x = (y' + x!) in x' * (x + \
       a)!\""
      (Binop
         ( Mult,
           Deriv (Binop (Add, Deriv (Var "y"), Fact (Var "x"))),
           Fact
             (Binop (Add, Binop (Add, Deriv (Var "y"), Fact (Var "x")), Var "a"))
         ))
      "let x = y' + x! in x' * (x + a)!";
    force_sub_test
      "force sub expression \"a - x * b + c * x ^ 2\" with binding (x, x + 1)"
      (Binop
         ( Add,
           Binop
             ( Add,
               Var "a",
               Binop
                 ( Mult,
                   Binop (Mult, Binop (Add, Var "x", Float 1.), Var "b"),
                   Float ~-.1. ) ),
           Binop
             ( Mult,
               Var "c",
               Binop (Pow, Binop (Add, Var "x", Float 1.), Float 2.) ) ))
      ("x", convert (Binop (Add, Var "x", Float 1.)))
      "a - x * b + c * x ^ 2";
    full_eval_test "fully evaluate expression \"0.1\"" (Float 0.1) "0.1";
    full_eval_test "fully evaluate expression \"let x = 1 in x + 1\"" (Float 2.)
      "let x = 1 in x + 1";
    full_eval_test
      "fully evaluate expression \"let x = 1 in let x = 2 + x in let x = x + x \
       in x + 1\""
      (Float 7.) "let x = 1 in let x = 2 + x in let x = x + x in x + 1";
    full_eval_test
      "fully evalaute expression \"let x = 0 in (Sin x + 2) ^ (2 + Cos (x + 2 \
       * Tan (x / 1) - 0))\""
      (Float 8.)
      "let x = 0 in (Sin x + 2) ^ (2 + Cos (x + 2 * Tan (x / 1) - 0))";
    full_eval_test
      "fully evaluate expression \"let x = 3! + 0! in (x - (logBase 7 49) * \
       (Sin x) ^ 0)!\""
      (Float 120.) "let x = 3! + 0! in (x - (logBase 7 49) * (Sin x) ^ 0)!";
    regenerate_euler_test
      "the population at time 0. N(0.) = 50., and dN/dt = let x = 6 - y in x * \
       10, where y is bound to value 1, then N(1.) = 100."
      (1., 100.) (0., 50.) 1.
      [ ("y", "1" |> to_expression |> convert) ]
      "let x = 6 - y in x * 10";
    regenerate_euler_test
      "the population at time 0. N(0.) = 50., and dN/dt = let y = 1 in let x = \
       6 - y in x * 10, then N(10.) = 550."
      (10., 550.) (0., 50.) 10.
      [ ("y", "1" |> to_expression |> convert) ]
      "let y = 1 in let x = 6 - y in x * 10";
  ]

(***************************************************************************
  Tests for model
  **************************************************************************)

(** [cmp_set_like_lst f lst1 lst2] is true if and only if both [lst1] and [lst2]
    are lists without any duplicates and contain the same elements. *)
let cmp_set_like_lst (lst1 : 'a list) (lst2 : 'a list) : bool =
  let new_lst1 = List.sort_uniq compare lst1 in
  let new_lst2 = List.sort_uniq compare lst2 in
  new_lst1 = new_lst2
  && List.length new_lst1 = List.length lst1
  && List.length new_lst2 = List.length lst2

(** [species_names_test name input_model expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output] with
    [species_names input_model]. *)
let species_names_test (name : string) (input_model : Model.model)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output (species_names input_model) ~cmp:cmp_set_like_lst

(** [member_species_test name input_species input_model expected_output]
    constructs an OUnit test named [name] that asserts the equality of
    [expected_output] with [member_species input_species input_model]. *)
let member_species_test (name : string) (input_species : string)
    (input_model : Model.model) (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (member_species input_species input_model)

(** [par_names_test name input_model expected_output] constructs an OUnit test
    named [name] that asserts the equality of [expected_output] with
    [par_names input_model]. *)
let par_names_test (name : string) (input_model : Model.model)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output (par_names input_model) ~cmp:cmp_set_like_lst

(** [member_par_test name input_par input_model expected_output] constructs an
    OUnit test named [name] that asserts the equality of [expected_output] with
    [member_par input_par input_model]. *)
let member_par_test (name : string) (input_par : string)
    (input_model : Model.model) (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (member_par input_par input_model)

(** [find_init_pop_test name input_name input_model expected_output] constructs
    an OUnit test named [name] that asserts the equality of [expected_output]
    with [find_init_pop input_name input_model]. *)
let find_init_pop_test (name : string) (input_name : string)
    (input_model : Model.model) (expected_output : float) : test =
  name >:: fun _ ->
  assert_equal expected_output (find_init_pop input_name input_model)

(** [find_init_pop_exn_test name input_name input_model] constructs an OUnit
    test named [name] that asserts [find_init_pop input_name input_model] raises
    [UnfoundSpecies]. *)
let find_init_pop_exn_test (name : string) (input_name : string)
    (input_model : Model.model) : test =
  name >:: fun _ ->
  assert_raises UnfoundSpecies (fun () -> find_init_pop input_name input_model)

(** [find_diff_eq_test name input_name input_model expected_output] constructs
    an OUnit test named [name] that asserts the equality of [expected_output]
    with [find_diff_eq input_name input_model]. *)
let find_diff_eq_test (name : string) (input_name : string)
    (input_model : Model.model) (expected_output : Ast.expr) : test =
  name >:: fun _ ->
  assert_equal expected_output (find_diff_eq input_name input_model)

(** [find_diff_eq_exn_test name input_name input_model] constructs an OUnit test
    named [name] that asserts [find_diff_eq input_name input_model] raises
    [UnfoundSpecies]. *)
let find_diff_eq_exn_test (name : string) (input_name : string)
    (input_model : Model.model) : test =
  name >:: fun _ ->
  assert_raises UnfoundSpecies (fun () -> find_diff_eq input_name input_model)

(** [find_duration_test name input_model expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output] with
    [find_duration input_model]. *)
let find_duration_test (name : string) (input_model : Model.model)
    (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (find_duration input_model)

(** [find_par_val_test name input_par input_model expected_output] constructs an
    OUnit test named [name] that asserts the equality of [expected_output] with
    [find_par_val input_par input_model]. *)
let find_par_val_test (name : string) (input_par : string)
    (input_model : Model.model) (expected_output : float) : test =
  name >:: fun _ ->
  assert_equal expected_output (find_par_val input_par input_model)

(** [find_par_val_exn_test name input_par input_model] constructs an OUnit test
    named [name] that asserts [find_par_val input_par input_model] raises
    [UnfoundPar]. *)
let find_par_val_exn_test (name : string) (input_par : string)
    (input_model : Model.model) : test =
  name >:: fun _ ->
  assert_raises UnfoundPar (fun () -> find_par_val input_par input_model)

(** [add_species_exn_test name input_name input_model] constructs an OUnit test
    named [name] that asserts [add_species input_name input_model] raises
    [DuplicateSpecies]. *)
let add_species_exn_test (name : string) (input_name : string)
    (input_model : Model.model) : test =
  name >:: fun _ ->
  assert_raises DuplicateSpecies (fun () -> add_species input_name input_model)

(** [add_diff_eq_exn_test name input_name input_diff_eq input_model] constructs
    an OUnit test named [name] that asserts
    [add_diff_eq input_name input_diff_eq input_model] raises [UnfoundSpecies]. *)
let add_diff_eq_exn_test (name : string) (input_name : string)
    (input_diff_eq : Ast.expr) (input_model : Model.model) : test =
  name >:: fun _ ->
  assert_raises UnfoundSpecies (fun () ->
      add_diff_eq input_name input_diff_eq input_model)

(** [add_init_pop_exn_test name input_name input_init_pop input_model]
    constructs an OUnit test named [name] that asserts
    [add_init_pop input_name input_init_pop input_model] raises
    [UnfoundSpecies]. *)
let add_init_pop_exn_test (name : string) (input_name : string)
    (input_init_pop : float) (input_model : Model.model) : test =
  name >:: fun _ ->
  assert_raises UnfoundSpecies (fun () ->
      add_init_pop input_name input_init_pop input_model)

(** [add_duration_exn_test name input_duration input_model] constructs an OUnit
    test named [name] that asserts [add_duration input_duration input_model]
    raises [InvalidDuration]. *)
let add_duration_exn_test (name : string) (input_duration : int)
    (input_model : Model.model) : test =
  name >:: fun _ ->
  assert_raises InvalidDuration (fun () ->
      add_duration input_duration input_model)

(** [add_par_exn_test name input_par input_model] constructs an OUnit test named
    [name] that asserts [add_par input_par input_model] raises [DuplicatePar]. *)
let add_par_exn_test (name : string) (input_par : string)
    (input_model : Model.model) : test =
  name >:: fun _ ->
  assert_raises DuplicatePar (fun () -> add_par input_par input_model)

(** [add_par_val_exn_test name input_par input_val input_model] constructs an
    OUnit test named [name] that asserts
    [add_par_val input_par input_val input_model] raises [UnfoundPar]. *)
let add_par_val_exn_test (name : string) (input_par : string)
    (input_val : float) (input_model : Model.model) : test =
  name >:: fun _ ->
  assert_raises UnfoundPar (fun () ->
      add_par_val input_par input_val input_model)

(** [graph_point_exn_test name msg m num_step acc prev_step prev step]
    constructs an OUnit test named [name] that asserts
    [graph_point m num_step acc prev_step prev step] raises Failure [msg]. *)
let graph_point_exn_test (name : string) (msg : string) (input_m : Model.model)
    (input_num_step : int) (input_acc : int)
    (input_prev_step : (float * float) list)
    (input_prev : (float * float) list list) (input_time_step : float) : test =
  name >:: fun _ ->
  assert_raises (Failure msg) (fun () ->
      graph_point input_m input_num_step input_acc input_prev_step input_prev
        input_time_step)

(** [graph_point_num_step_exn_test name m num_step acc prev_step prev step]
    constructs an OUnit test named [name] that asserts
    [graph_point m num_step acc prev_step prev step] raises Failure "the number
    of steps is negative". *)
let graph_point_num_step_exn_test (name : string) (input_m : Model.model)
    (input_num_step : int) (input_acc : int)
    (input_prev_step : (float * float) list)
    (input_prev : (float * float) list list) (input_time_step : float) : test =
  graph_point_exn_test name "the number of steps is negative" input_m
    input_num_step input_acc input_prev_step input_prev input_time_step

(** [graph_point_acc2_exn_test name m num_step acc prev_step prev step]
    constructs an OUnit test named [name] that asserts
    [graph_point m num_step acc prev_step prev step] raises Failure "the number
    of steps that have been gone through is negative". *)
let graph_point_acc1_exn_test (name : string) (input_m : Model.model)
    (input_num_step : int) (input_acc : int)
    (input_prev_step : (float * float) list)
    (input_prev : (float * float) list list) (input_time_step : float) : test =
  graph_point_exn_test name
    "the number of steps that have been gone through is negative" input_m
    input_num_step input_acc input_prev_step input_prev input_time_step

(** [graph_point_acc1_exn_test name m num_step acc prev_step prev step]
    constructs an OUnit test named [name] that asserts
    [graph_point m num_step acc prev_step prev step] raises Failure "the number
    of steps that have been gone through is negative". *)
let graph_point_acc2_exn_test (name : string) (input_m : Model.model)
    (input_num_step : int) (input_acc : int)
    (input_prev_step : (float * float) list)
    (input_prev : (float * float) list list) (input_time_step : float) : test =
  graph_point_exn_test name
    "the number of steps is less than the number of steps that have been gone \
     through"
    input_m input_num_step input_acc input_prev_step input_prev input_time_step

(** [string_of_lst lst] is a string representing a point list [lst]. Example:
    [string_of_lst \[(1., 2.); (3., 4.); (5., 6.)\]] is "(1., 2.); (3., 4.);
    (5., 6.); []\n". [string_of_lst \[\]] is "[]\n". *)
let rec string_of_lst (lst : (float * float) list) =
  match lst with
  | [] -> "[]\n"
  | (x, y) :: t ->
      "(" ^ string_of_float x ^ ", " ^ string_of_float y ^ "); "
      ^ string_of_lst t

(** [string_of_matrix m] is a string representing a point list list [m].
    Example:
    [string_of_matrix \[\[(1., 2.); (3., 4.)\]; \[(5., 6.); (7., 8.)\]\]] is
    "(1., 2.); (3., 4.); []\n(5., 6.); (7., 8.); []\n[]\n".
    [string_of_matrix \[\]] is "[]\n". *)
let rec string_of_matrix (matrix : (float * float) list list) =
  match matrix with
  | [] -> "[]\n"
  | h :: t -> string_of_lst h ^ string_of_matrix t

(** [cmp_float_approx d f1 f2] is true iff [f1] and [f2] are the same after
    being round to the digit of 1-based index [d] after the decimal point. *)
let cmp_float_approx (digit : int) (f1 : float) (f2 : float) : bool =
  let ampf (f : float) =
    f *. Float.pow 10. (float_of_int digit) |> Float.round
  in
  let ampf_f1 = ampf f1 in
  let ampf_f2 = ampf f2 in
  ampf_f1 = ampf_f2

(** [cmp_lst_approx d lst1 lst2] is true iff [lst1] and [lst2] have the same
    length [n], and for all index [i] from [0] to [n - 1], let the element of
    index [i] for [lst1] and [lst2] be [(x1, y1)] and [(x2, y2)], then
    [cmp_float_approx d x1 x2] and [cmp_float_approx d y1 y2] are both true. *)
let rec cmp_lst_approx (digit : int) (lst1 : (float * float) list)
    (lst2 : (float * float) list) : bool =
  match (lst1, lst2) with
  | [], [] -> true
  | _ :: _, [] | [], _ :: _ -> false
  | (h11, h12) :: t1, (h21, h22) :: t2 ->
      cmp_float_approx digit h11 h21
      && cmp_float_approx digit h12 h22
      && cmp_lst_approx digit t1 t2

(** [cmp_matrix_approx d m1 m2] is true iff [m1] and [m2] have the same length
    [n], and for all index [i] from [0] to [n - 1], let the element of index [i]
    for [m1] and [m2] be [lst1] and [lst2], then [cmp_lst_approx d x1 x2] is
    true.*)
let rec cmp_matrix_approx (digit : int) (m1 : (float * float) list list)
    (m2 : (float * float) list list) : bool =
  match (m1, m2) with
  | [], [] -> true
  | _ :: _, [] | [], _ :: _ -> false
  | h1 :: t1, h2 :: t2 ->
      cmp_lst_approx digit h1 h2 && cmp_matrix_approx digit t1 t2

(** [graph_point_test name d expected_output m num_step acc prev_step prev step]
    constructs an OUnit test named [name] that asserts the equality of
    [expected_output] and [graph_point m num_step acc prev_step prev step] to
    the digit of 1-based index [d] after the decimal points. *)
let graph_point_test (name : string) (digit : int option)
    (expected_output : (float * float) list list) (input_m : Model.model)
    (input_num_step : int) (input_acc : int)
    (input_prev_step : (float * float) list)
    (input_prev : (float * float) list list) (input_time_step : float) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (graph_point input_m input_num_step input_acc input_prev_step input_prev
       input_time_step)
    ~printer:string_of_matrix
    ~cmp:
      (let f = function
         | None -> fun x y -> x = y
         | Some d -> cmp_matrix_approx d
       in
       f digit)

let init_m = init_model ()

let single_species =
  init_m |> add_species "N1"
  |> add_diff_eq "N1" (to_expression "a11 * N1")
  |> add_init_pop "N1" 100.

let two_species =
  add_species "N2" single_species
  |> add_diff_eq "N2" (to_expression "a22 * N2")
  |> add_init_pop "N2" 200.

let two_species_N1_update =
  two_species
  |> add_diff_eq "N1" (to_expression "a11 * N1 + a12 * N2")
  |> add_init_pop "N1" 1000.

let three_species = add_species "Wolf" two_species
let one_par_two_species = add_par "a11" two_species |> add_par_val "a11" 0.15

let two_par_two_species =
  add_par "a12" one_par_two_species |> add_par_val "a12" (-0.5)

let three_par_two_species = add_par "a21" two_par_two_species

let two_par_two_species_a11_update =
  two_par_two_species |> add_par_val "a11" 0.2

let dur_10 = init_m |> add_duration 10
let dur_100 = dur_10 |> add_duration 100
let dur_0 = dur_100 |> add_duration 0

let lotka_volterra_model =
  init_m |> add_species "N1" |> add_species "N2" |> add_par "a11"
  |> add_par "a12" |> add_par "a21" |> add_par "a22"
  |> add_diff_eq "N1" (to_expression "a11 * N1 + a12 * N2")
  |> add_diff_eq "N2" (to_expression "a21 * N1 + a22 * N2")
  |> add_par_val "a11" 0.01 |> add_par_val "a12" ~-.0.02
  |> add_par_val "a21" 0.05 |> add_par_val "a22" ~-.0.03
  |> add_init_pop "N1" 200. |> add_init_pop "N2" 100. |> add_duration 100

let algae_chemostat_model =
  init_m |> add_species "N" |> add_species "C" |> add_species "D"
  |> add_species "T" |> add_par "d" |> add_par "V" |> add_par "NI"
  |> add_par "c1" |> add_par "c2" |> add_par "b1" |> add_par "b2"
  |> add_par "K1" |> add_par "K2" |> add_par "m" |> add_par "p"
  |> add_diff_eq "N"
       (to_expression
          "d * V * NI - C * c1 * b1 * N / V / (K1 + N / V) - D * c2 * b2 * N / \
           V / (K2 + N / V) - d * N")
  |> add_diff_eq "C"
       (to_expression
          "C * c1 * b1 * N / V / (K1 + N / V) - m * C * T / V - d * C")
  |> add_diff_eq "D"
       (to_expression
          "D * c2 * b2 * N / V / (K2 + N / V) - p * D * c2 * b2 * N / V / (K2 \
           + N / V) - d * D")
  |> add_diff_eq "T"
       (to_expression "p * D * c2 * b2 * N / V / (K2 + N / V) - d * T")
  |> add_par_val "d" 0.5 |> add_par_val "V" 100. |> add_par_val "NI" 0.005
  |> add_par_val "c1" 100. |> add_par_val "c2" 200. |> add_par_val "b1" 0.005
  |> add_par_val "b2" 0.002 |> add_par_val "K1" 1. |> add_par_val "K2" 2.
  |> add_par_val "m" 0.03 |> add_par_val "p" 0.1 |> add_init_pop "N" 10.
  |> add_init_pop "C" 5. |> add_init_pop "D" 10. |> add_init_pop "T" 0.
  |> add_duration 100

let geometric_model =
  init_m |> add_species "N"
  |> add_diff_eq "N" (to_expression "99 * N")
  |> add_init_pop "N" 1. |> add_duration 1000

let infectious_disease_model =
  init_m |> add_species "S" |> add_species "I" |> add_species "R" |> add_par "b"
  |> add_par "m" |> add_par "n" |> add_par "g"
  |> add_diff_eq "S" (to_expression "- b * S * I - m * S")
  |> add_diff_eq "I" (to_expression "b * S * I - (m + n) * I - g * I")
  |> add_diff_eq "R" (to_expression "g * I - m * R")
  |> add_par_val "b" 0.0001 |> add_par_val "m" 0.0001 |> add_par_val "n" 0.001
  |> add_par_val "g" 0.005 |> add_init_pop "S" 500. |> add_init_pop "I" 10.
  |> add_init_pop "R" 0. |> add_duration 1

let model_tests =
  [
    species_names_test
      "the list of names of species in an initial empty model is []" init_m [];
    species_names_test
      "the list of names of species in a model with one species named \"N1\" \
       is [\"N1\"]"
      single_species [ "N1" ];
    species_names_test
      "the list of names of species in a model with two species named \"N1\" \
       and \"N2\" is [\"N1\"; \"N2\"]"
      two_species [ "N1"; "N2" ];
    member_species_test
      "\"N1\" is not the name of any species in the initial empty model" "N1"
      init_m false;
    member_species_test
      "\"N1\" is the name of a species in the model with one species named \
       \"N1\""
      "N1" single_species true;
    member_species_test
      "\"N2\" is not the name of any species in the model with one species \
       named \"N1\""
      "N2" single_species false;
    member_species_test
      "\"N1\" is the name of a species in the model with two species named \
       \"N1\" and \"N2\""
      "N1" two_species true;
    member_species_test
      "\"N2\" is the name of a species in the model with two species named \
       \"N1\" and \"N2\""
      "N2" two_species true;
    member_species_test
      "\"N3\" is the name of a species in the model with two species named \
       \"N1\" and \"N2\""
      "N3" two_species false;
    par_names_test
      "the list of names of parameters in the two_species model is []"
      two_species [];
    par_names_test
      "the list of names of parameters in the one_par_two_species model is \
       [\"a11\"]"
      one_par_two_species [ "a11" ];
    par_names_test
      "the list of names of parameters in the two_par_two_species model is \
       [\"a11\"; \"a12\"]"
      two_par_two_species [ "a11"; "a12" ];
    member_par_test
      "\"a11\" is not the name of any parameters in the initial empty model"
      "a1" init_m false;
    member_par_test
      "\"a11\" is the name of a parameter in the one_par_two_species model"
      "a11" one_par_two_species true;
    member_par_test
      "\"a12\" is not the name of any parameters in the one_par_two_species \
       model"
      "a12" one_par_two_species false;
    member_par_test
      "\"a11\" is the name of a parameter in the two_par_two_species model"
      "a11" two_par_two_species true;
    member_par_test
      "\"a12\" is the name of a parameter in the two_par_two_species model"
      "a12" two_par_two_species true;
    member_par_test
      "\"a21\" is not the name of any parameters in the two_par_two_species \
       model"
      "a21" two_par_two_species false;
    find_init_pop_test
      "the initial population for species \"N1\" in the model [single_species] \
       is 100."
      "N1" single_species 100.;
    find_init_pop_test
      "the initial population for species \"N1\" in the model [two_species] is \
       100."
      "N1" two_species 100.;
    find_init_pop_test
      "the initial population for species \"N2\" in the model [two_species] is \
       200."
      "N2" two_species 200.;
    find_init_pop_test
      "the initial population for species \"N1\" in the model \
       [two_species_N1_update] is 1000."
      "N1" two_species_N1_update 1000.;
    find_init_pop_test
      "the initial population for species \"N2\" in the model \
       [two_species_N1_update] is 200."
      "N2" two_species_N1_update 200.;
    find_init_pop_test
      "the initial population for species \"Wolf\" in the model \
       [three_species] is 0."
      "Wolf" three_species 0.;
    find_init_pop_exn_test
      "trying to find the initial population of species \"N1\" in the initial \
       empty model raises [UnfoundSpecies]"
      "N1" init_m;
    find_init_pop_exn_test
      "trying to find the initial population of species \"N2\" in model \
       [single_species] raises [UnfoundSpecies]"
      "N2" single_species;
    find_init_pop_exn_test
      "trying to find the initial population of species \"N\" in model \
       [two_species] raises [UnfoundSpecies]"
      "N" two_species;
    find_diff_eq_test
      "the diff eq for species \"N1\" in model [single_species] is \"a11 * N1\""
      "N1" single_species (to_expression "a11 * N1");
    find_diff_eq_test
      "the diff eq for species \"N1\" in model [two_species] is \"a11 * N1\""
      "N1" two_species (to_expression "a11 * N1");
    find_diff_eq_test
      "the diff eq for species \"N2\" in model [two_species] is \"a22 * N2\""
      "N2" two_species (to_expression "a22 * N2");
    find_diff_eq_test
      "the diff eq for species \"N1\" in model [two_species_N1_update] is \
       \"a11 * N1 + a12 * N2\""
      "N1" two_species_N1_update
      (to_expression "a11 * N1 + a12 * N2");
    find_diff_eq_test
      "the diff eq for species \"N2\" in model [two_species_N1_update] is \
       \"a22 * N2\""
      "N2" two_species_N1_update (to_expression "a22 * N2");
    find_diff_eq_test
      "the diff eq for species \"Wolf\" in model [three_species] is \"0.\""
      "Wolf" three_species (to_expression "0.");
    find_diff_eq_exn_test
      "trying to find the diff eq of species \"N1\" in the initial empty model \
       raises [UnfoundSpecies]"
      "N1" init_m;
    find_diff_eq_exn_test
      "trying to find the diff eq of species \"N2\" in model [single_species] \
       raises [UnfoundSpecies]"
      "N2" single_species;
    find_diff_eq_exn_test
      "trying to find the initial population of species \"N\" in model \
       [two_species] raises [UnfoundSpecies]"
      "N" two_species;
    find_duration_test
      "the duration for simulation in the initial empty model is 0" init_m 0;
    find_duration_test "the duration for simulation in the model dur_10 is 10"
      dur_10 10;
    find_duration_test "the duration for simulation in the model dur_100 is 100"
      dur_100 100;
    find_duration_test "the duration for simulation in the model dur_0 is 0"
      dur_0 0;
    find_par_val_test
      "the value of parameter \"a11\" in model [one_par_two_species] is 0.15"
      "a11" one_par_two_species 0.15;
    find_par_val_test
      "the value of parameter \"a11\" in model [two_par_two_species] is 0.15"
      "a11" two_par_two_species 0.15;
    find_par_val_test
      "the value of parameter \"a12\" in model [two_par_two_species] is -0.5"
      "a12" two_par_two_species ~-.0.5;
    find_par_val_test
      "the value of parameter \"a11\" in model \
       [two_par_two_species_a11_update] is 0.2"
      "a11" two_par_two_species_a11_update 0.2;
    find_par_val_test
      "the value of parameter \"a12\" in model \
       [two_par_two_species_a11_update] is -0.5"
      "a12" two_par_two_species_a11_update ~-.0.5;
    find_par_val_test
      "the value of parameter \"a21\" in model [three_par_two_species] is 0."
      "a21" three_par_two_species 0.;
    find_par_val_exn_test
      "trying to find the value of parameter \"N1\" in the initial empty model \
       raises [UnfoundPar]"
      "N1" init_m;
    find_par_val_exn_test
      "trying to find the the value of parameter \"N1\" in model \
       [one_par_two_species] raises [UnfoundPar]"
      "N1" one_par_two_species;
    find_par_val_exn_test
      "trying to find the the value of parameter \"a22\" in model \
       [two_par_two_species] raises [UnfoundPar]"
      "a22" two_par_two_species;
    add_species_exn_test
      "trying to add species \"N1\" to model [single_species] raises \
       [DuplicateSpecies]"
      "N1" single_species;
    add_species_exn_test
      "trying to add species \"N1\" to model [two_species] raises \
       [DuplicateSpecies]"
      "N1" two_species;
    add_species_exn_test
      "trying to add species \"N2\" to model [two_species] raises \
       [DuplicateSpecies]"
      "N2" two_species;
    add_diff_eq_exn_test
      "trying to add diff eq \"a11 * N1 + a12 * N2\" to species \"N1\" in the \
       initial empty model raises [UnfoundSpecies]"
      "N1"
      (to_expression "a11 * N1 + a12 * N2")
      init_m;
    add_diff_eq_exn_test
      "trying to add diff eq \"a21 * N1 + a22 * N2\" to species \"N2\" in the \
       model [single_species] raises [UnfoundSpecies]"
      "N2"
      (to_expression "a21 * N1 + a22 * N2")
      single_species;
    add_diff_eq_exn_test
      "trying to add diff eq \"a31 * N1 + a32 * N2 + a33 * N3\" to species \
       \"N3\" in the model [two_species] raises [UnfoundSpecies]"
      "N3"
      (to_expression "a31 * N1 + a32 * N2 + a33 * N3")
      two_species;
    add_init_pop_exn_test
      "trying to add initial population 100. to species \"N1\" in the initial \
       empty model raises [UnfoundSpecies]"
      "N1" 100. init_m;
    add_init_pop_exn_test
      "trying to add initial population 200. to species \"N2\" in the model \
       [single_species] raises [UnfoundSpecies]"
      "N2" 200. single_species;
    add_init_pop_exn_test
      "trying to add initial population 1000. to species \"N3\" in the model \
       [two_species] raises [UnfoundSpecies]"
      "N3" 1000. two_species;
    add_duration_exn_test
      "trying to add duration -1 to model dur_100 raises [InvalidDuration]" ~-1
      dur_100;
    add_duration_exn_test
      "trying to add duration -100 to model dur_100 raises [InvalidDuration]"
      ~-100 dur_100;
    add_par_exn_test
      "trying to add parameter \"a11\" to model [one_par_two_species] raises \
       [DuplicatePar]"
      "a11" one_par_two_species;
    add_par_exn_test
      "trying to add parameter \"a11\" to model [two_par_two_species] raises \
       [DuplicatePar]"
      "a11" two_par_two_species;
    add_par_exn_test
      "trying to add parameter \"a12\" to model [two_par_two_species] raises \
       [DuplicatePar]"
      "a12" two_par_two_species;
    add_par_val_exn_test
      "trying to add value 0.5 to parameter \"a11\" in the inital empty model \
       raises [UnfoundPar]"
      "a11" 0.5 init_m;
    add_par_val_exn_test
      "trying to add value 0.5 to parameter \"a12\" in model \
       [one_par_two_species] raises [UnfoundPar]"
      "a12" 0.5 one_par_two_species;
    add_par_val_exn_test
      "trying to add value 0.5 to parameter \"a22\" in model \
       [two_par_two_species] raises [UnfoundPar]"
      "a22" 0.5 two_par_two_species;
    graph_point_num_step_exn_test
      "-1 number of steps fails with \"the number of steps is negative\""
      lotka_volterra_model ~-1 0 [] [] 10.;
    graph_point_num_step_exn_test
      "min_int number of steps fails with \"the number of steps is negative\""
      lotka_volterra_model min_int 0 [] [] 10.;
    graph_point_acc1_exn_test
      "acc being -1 fails with \"the number of steps that have been gone \
       through is negative\""
      lotka_volterra_model 10 ~-1 [] [] 10.;
    graph_point_acc1_exn_test
      "acc being min_int fails with \"the number of steps that have been gone \
       through is negative\""
      lotka_volterra_model 100 min_int [] [] 10.;
    graph_point_acc2_exn_test
      "num_step being 1 and acc being 5 fails with \"the number of steps is \
       less than the number of steps that have been gone through\""
      lotka_volterra_model 1 5 [] [] 10.;
    graph_point_acc2_exn_test
      "num_step being 100 and acc being max_int fails with \"the number of \
       steps is less than the number of steps that have been gone through\""
      lotka_volterra_model 100 max_int [] [] 10.;
    graph_point_test
      "running the lotka-volterra_model_1 from time = 0 for 0 step with step \
       length = 1 gives: N1(0) = 200., N2(0) = 100."
      None
      [ [ (0., 200.); (0., 100.) ] ]
      lotka_volterra_model 0 0 [] [] 1.;
    graph_point_test
      "running the lotka-volterra_model_1 from time = 0 for 1 step with step \
       length = 1 gives: N1(0) = 200., N2(0) = 100."
      None
      [ [ (0., 200.); (0., 100.) ] ]
      lotka_volterra_model 1 0 [] [] 1.;
    graph_point_test
      "running the lotka-volterra_model_1 from time = 0 for 2 steps with step \
       length = 1 gives: N1(0) = 200., N2(0) = 100., N1(1) = 200., N2(1) = \
       107."
      None
      [ [ (0., 200.); (0., 100.) ]; [ (1., 200.); (1., 107.) ] ]
      lotka_volterra_model 2 0 [] [] 1.;
    graph_point_test
      "running the lotka-volterra_model_1 from time = 0 for 2 steps with step \
       length = 0.1 gives: N1(0) = 200., N2(0) = 100., N1(1) = 200., N2(1) = \
       100.7."
      None
      [ [ (0., 200.); (0., 100.) ]; [ (0.1, 200.); (0.1, 100.7) ] ]
      lotka_volterra_model 2 0 [] [] 0.1;
    graph_point_test
      "running the lotka-volterra_model_1 from time = 0 for 3 steps with step \
       length = 1 gives: N1(0) = 200., N2(0) = 100., N1(1) = 200., N2(1) = \
       107., N1(2) = 199.86, N2(2) = 113.79."
      None
      [
        [ (0., 200.); (0., 100.) ];
        [ (1., 200.); (1., 107.) ];
        [ (2., 199.86); (2., 113.79) ];
      ]
      lotka_volterra_model 3 0 [] [] 1.;
    graph_point_test
      "running the lotka-volterra_model_1 from time = 0 for 2 steps with step \
       length = 0.1 gives: N1(0) = 200., N2(0) = 100., N1(1) = 200., N2(1) = \
       100.7."
      None
      [
        [ (0., 200.); (0., 100.) ];
        [ (0.1, 200.); (0.1, 100.7) ];
        [ (0.2, 199.9986); (0.2, 101.3979) ];
      ]
      lotka_volterra_model 3 0 [] [] 0.1;
    graph_point_test
      "running the algae_chemostat_model from time = 0 for 0 step with step \
       length = 1 gives: N(0) = 10., C(0) = 5., D(0) = 10., T(0) = 0."
      None
      [ [ (0., 10.); (0., 5.); (0., 10.); (0., 0.) ] ]
      algae_chemostat_model 0 0 [] [] 1.;
    graph_point_test
      "running the algae_chemostat_model from time = 0 for 1 step with step \
       length = 1 gives: N(0) = 10., C(0) = 5., D(0) = 10., T(0) = 0."
      None
      [ [ (0., 10.); (0., 5.); (0., 10.); (0., 0.) ] ]
      algae_chemostat_model 1 0 [] [] 1.;
    graph_point_test
      "running the algae_chemostat_model from time = 0 for 2 steps with step \
       length = 1 gives: N(0) = 10., C(0) = 5., D(0) = 10., T(0) = 0., N(1) = \
       4.832251082, C(1) = 2.727272727, D(1) = 5.171428571, T(1) = 0."
      (Some 9)
      [
        [ (0., 10.); (0., 5.); (0., 10.); (0., 0.) ];
        [
          (1., 4.832251082);
          (1., 2.727272727);
          (1., 5.171428571);
          (1., 0.01904761905);
        ];
      ]
      algae_chemostat_model 2 0 [] [] 1.;
    graph_point_test
      "running the algae_chemostat_model from time = 0 for 11 steps with step \
       length = 1"
      (Some 6)
      [
        [ (0., 10.); (0., 5.); (0., 10.); (0., 0.) ];
        [ (1., 4.832251); (1., 2.727273); (1., 5.171429); (1., 0.019048) ];
        [ (2., 2.554468); (2., 1.426478); (2., 2.629634); (2., 0.014404) ];
        [ (3., 1.496203); (3., 0.730998); (3., 1.326756); (3., 0.008528) ];
        [ (4., 0.988773); (4., 0.370885); (4., 0.666925); (4., 0.004658) ];
        [ (5., 0.741258); (5., 0.187258); (5., 0.334643); (5., 0.002460) ];
        [ (6., 0.619446); (6., 0.094318); (6., 0.167767); (6., 0.001280) ];
        [ (7., 0.559225); (7., 0.047449); (7., 0.084070); (7., 0.000661) ];
        [ (8., 0.529387); (8., 0.023856); (8., 0.042119); (8., 0.000340) ];
        [ (9., 0.514586); (9., 0.011991); (9., 0.021100); (9., 0.000174) ];
        [ (10., 0.507241); (10., 0.006026); (10., 0.010569); (10., 0.000089) ];
      ]
      algae_chemostat_model 11 0 [] [] 1.;
    graph_point_test
      "running the geometric_model from time = 0 for 0 step with step length = \
       1 gives: N(0) = 1."
      None
      [ [ (0., 1.) ] ]
      geometric_model 0 0 [] [] 1.;
    graph_point_test
      "running the geometric_model from time = 0 for 1 step with step length = \
       1 gives: N(0) = 1."
      None
      [ [ (0., 1.) ] ]
      geometric_model 1 0 [] [] 1.;
    graph_point_test
      "running the geometric_model from time = 0 for 5 steps with step length \
       = 1 gives: N(0) = 1., N(1) = 100., N(2) = 10000., N(3) = 1000000., N(4) \
       = 100000000."
      None
      [
        [ (0., 1.) ];
        [ (1., 100.) ];
        [ (2., 10000.) ];
        [ (3., 1000000.) ];
        [ (4., 100000000.) ];
      ]
      geometric_model 5 0 [] [] 1.;
    graph_point_test
      "running the geometric_model from time = 0 for 1000 steps with step \
       length = 1 gives [] as the system breaks down in computing numbers \
       larger than the bound."
      None [] geometric_model 1000 0 [] [] 1.;
    graph_point_test
      "running the infectious_disease_model from time = 0 for 11 steps with \
       step length = 1"
      (Some 6)
      [
        [ (0., 500.); (0., 10.); (0., 0.) ];
        [ (1., 499.45); (1., 10.439); (1., 0.05) ];
        [ (2., 498.878679); (2., 10.896698); (2., 0.10219) ];
        [ (3., 498.285178); (3., 11.373841); (3., 0.156663) ];
        [ (4., 497.668608); (4., 11.871202); (4., 0.213517) ];
        [ (5., 497.028049); (5., 12.389580); (5., 0.272851) ];
        [ (6., 496.362549); (6., 12.929801); (6., 0.334772) ];
        [ (7., 495.671126); (7., 13.492716); (7., 0.399388) ];
        [ (8., 494.952764); (8., 14.079205); (8., 0.466811) ];
        [ (9., 494.206414); (9., 14.690176); (9., 0.537161) ];
        [ (10., 493.430996); (10., 15.326564); (10., 0.610558) ];
      ]
      infectious_disease_model 11 0 [] [] 1.;
    graph_point_test
      "running the infectious_disease_model from time = 0 for 21 steps with \
       step length = 5"
      (Some 6)
      [
        [ (0., 500.); (0., 10.); (0., 0.) ];
        [ (5., 497.25); (5., 12.195); (5., 0.25) ];
        [ (10., 493.969393); (10., 14.855034); (10., 0.55475) ];
        [ (15., 490.053442); (15., 18.070922); (15., 0.925848) ];
        [ (20., 485.380557); (20., 21.947618); (20., 1.377159) ];
        [ (25., 479.811393); (25., 26.604689); (25., 1.925160) ];
        [ (30., 473.188871); (30., 32.175862); (30., 2.589315) ];
        [ (35., 465.339647); (35., 38.807128); (35., 3.392417) ];
        [ (40., 456.077729); (40., 46.652758); (40., 4.360899) ];
        [ (45., 445.211048); (45., 55.868491); (45., 5.525038) ];
        [ (50., 432.551808); (50., 66.601137); (50., 6.918987) ];
        [ (55., 417.931311); (55., 78.974024); (55., 8.580556) ];
        [ (60., 401.219487); (60., 93.068175); (60., 10.550617) ];
        [ (65., 382.348494); (65., 108.899978); (65., 12.872046) ];
        [ (70., 361.338449); (70., 126.397400); (70., 15.588109) ];
        [ (75., 338.321659); (75., 145.378399); (75., 18.740250) ];
        [ (80., 313.560168); (80., 165.536689); (80., 22.365340) ];
        [ (85., 287.450532); (85., 186.440676); (85., 26.492574) ];
        [ (90., 260.510571); (90., 207.550471); (90., 31.140345) ];
        [ (95., 233.345770); (95., 228.254727); (95., 36.313537) ];
        [ (100., 206.597959); (100., 247.924096); (100., 42.001748) ];
      ]
      infectious_disease_model 21 0 [] [] 5.;
    graph_point_test
      "running the infectious_disease_model from time = 0 for 10000 steps with \
       step length = 100. gives [] as the system breaks down in computing \
       numbers larger than the bound."
      None [] infectious_disease_model 10000 0 [] [] 100.;
  ]

(***************************************************************************
  Tests for gui
  **************************************************************************)

let add_point_test (name : string) (g : graph) (p : float)
    (expected_output : float list) =
  name >:: fun _ -> assert_equal (add_point p g |> points) expected_output

let name_test (name : string) (g : graph) (expected_output : string) =
  name >:: fun _ -> assert_equal (Guigraph.name g) expected_output

let step_test (name : string) (g : graph) (expected_output : float) =
  name >:: fun _ -> assert_equal (Guigraph.step g) expected_output

let min_max_multi_test (name : string) (mg : multigraph)
    (expected_output : float * float) =
  name >:: fun _ -> assert_equal (min_multi mg, max_multi mg) expected_output

let multi_test_1 =
  init_multigraph [ 0.0; 0.0 ] "My Test Multigraph" 0.1 [ "Wolf A"; "Wolf B" ]
    [ (0.1, 0.2, 0.3); (0.3, 0.4, 0.5) ]
  |> add_points_assoc [ ("Wolf A", 3.0); ("Wolf B", 4.0) ]
  |> add_points [ 6.0; 5.0 ]
  |> add_points [ 2.0; -5.0 ]

let multi_test_2 = multi_test_1 |> add_points [ -7.0; 90. ]

let gui_graph_tests =
  [
    add_point_test
      "Adding the point 0.5 to an empty graph gets the graph with points [0.5]."
      (empty_graph 0.1 (0., 0., 0.) "")
      0.5 [ 0.5 ];
    add_point_test
      "Adding the point 0.5 then 0.3 to an empty graph gets the graph with \
       points [0.5; 0.3]."
      (empty_graph 0.1 (0., 0., 0.) "" |> add_point 0.5)
      0.3 [ 0.5; 0.3 ];
    add_point_test
      "Adding the point 0.5 then 0.3 then -0.6 to an empty graph gets the \
       graph with points [0.5; 0.3; 0.6]."
      (empty_graph 0.1 (0., 0., 0.) "" |> add_point 0.5 |> add_point 0.3)
      ~-.0.6 [ 0.5; 0.3; -0.6 ];
    name_test "The name of graph with name Wolf A"
      (empty_graph 0.2 (0., 0., 0.) "Wolf A")
      "Wolf A";
    step_test "The delta t of graph with delta t 0.2"
      (empty_graph 0.2 (0., 0., 0.) "Wolf A")
      0.2;
    min_max_multi_test "Min max in wolf test" multi_test_1 (-5.0, 6.0);
    min_max_multi_test "Min max in wolf test with new mins and maxes added"
      multi_test_2 (-7.0, 90.);
  ]

let tests =
  "final project test suite"
  >::: List.flatten [ parse_tests; eval_tests; model_tests; gui_graph_tests ]

let _ = run_test_tt_main tests