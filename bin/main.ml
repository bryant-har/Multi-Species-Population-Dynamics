open Simulator
open Model
open Expression
open Eval
open Guigraph
open Gui_maker

let invalid_var_name_message =
  "The name you entered is invalid. It is not an expression. The name of a \
   species should be either a string of letters or a string of letters \
   followed by s string of numbers. Example: \"wolf\", \"Wolf1\", \"N12\"\n"

let inappropriate_var_name_message =
  "The name you entered is not an appropriate species name. The name of a \
   species should be either a string of letters or a string of letters \
   followed by s string of numbers. Example: \"wolf\", \"Wolf1\", \"N12\"\n"

let duplicate_var_name_message =
  "There is already a species with the same name in your model.\n"

let get_num_par_message =
  "Please enter the number of parameters you want to include in the model."

let invalid_par_name_message =
  "The name you entered is invalid. It is not an expression. The name of a \
   parameter should be either a string of letters or a string of letters \
   followed by s string of numbers. Example: \"b\", \"alpha\", \"a11\"\n"

let inappropriate_par_name_message =
  "The name you entered is not an appropriate parameter name. The name of a \
   parameter should be either a string of letters or a string of letters \
   followed by s string of numbers. Example: \"b\", \"alpha\", \"a11\"\n"

let duplicate_par_name_message =
  "There is already a parameter with the same name in your model.\n"

let duplicate_par_var_name_message =
  "There is already a species with the same name as the parameter in your model.\n"

let diff_eqs_rule_message =
  "The allowed differential equations may include any of the following: \n\n\
  \  - Operands: \n\
  \  1. The name of a species or a parameter that you have entered. \n\
  \  2. An integer or a float. Usage Example: 2, 2., 0.2, .2, 345.7801 \n\n\
  \  - Operators: \n\
  \  1. Add, Minus, Times, Divide. Usage Example: 1 + 2.0, N1 - N2 * 0.5, G / \
   b1 + 10.35 \n\
  \  2. Unary Minus. Usage Example: -1; -N1 \n\
  \  3. Power. [a ^ b] is [a] raised to the power of [b]. Usage Example: 2 ^ \
   3, N1 ^ 2 \n\
  \  4. Factorial. Usage Example: 10!, k! \n\
  \  5. Log. [logBase a b] is the log of [b] with base [a]. Usage Example: \
   logBase 10 N1 \n\
  \  6. Sine, Cosine, Tangent. Usage Example: Sin N, Cos 2.89, Tan 0.2 \n\
  \ \n\
  \  - Others: \n\
  \  1. Parentheses. Usage Example: (N1 + N2) / 0.5, Sin (N1 * a11 + N2 * a12) \n\
  \  2. Let. [let x = e1 in e2] is the expression that is obatined when all \
   the [x] in [e2] are replaced with [e1]. Use Example: let a = 2 in a + N * a \n"

let unbound_name_message =
  "The differential equation you entered includes an unbound name. \n"

let invalid_diff_eq_name_message =
  "The differential equation you entered is invalid. It is not an expression. \n"

let review_rule_message =
  "Do you want to review the rules for valid expressions for differential \
   equations? Type \"y\" for yes and \"n\" for no."

let invalid_choice_message =
  "What you entered is not a valid choice. You have to enter either \"y\" or \
   \"n\". \n"

let part_1_title =
  "==================================================\n\
   Part I: Enter Your Species\n\
   ==================================================\n"

let part_2_title =
  "\n\
   ==================================================\n\
   Part II: Enter Your Parameters\n\
   ==================================================\n"

let part_3_title =
  "\n\
   ==================================================\n\
   Part III: Enter the Differential Equations\n\
   ==================================================\n"

let part_4_title =
  "\n\
   ==================================================\n\
   Part IV: Enter Parameter Values\n\
   ==================================================\n"

let part_5_title =
  "\n\
   ==================================================\n\
   Part V: Enter Initial Populations\n\
   ==================================================\n"

let part_6_title =
  "\n\
   ==================================================\n\
   Part VI: Enter Simulation Parameters\n\
   ==================================================\n"

let part_7_title =
  "\n\
   ==================================================\n\
   Part VII: Simulation\n\
   ==================================================\n"

(** [launch_graphs m timestep point_matrix] launches the graphs for the
    simulation. *)
let launch_graphs (m : Model.model) (timestep : float)
    (point_matrix : (float * float) list list) : unit =
  let species_names = species_names m in
  let n_species = List.length species_names in
  let init_pops = List.map snd (List.hd point_matrix) in
  (* generate a list of pairs of multigraph and index, each corresponding to a
     species *)
  let rec init_graph_lst id graph_lst : (multigraph * int) list =
    if id >= n_species then graph_lst
    else
      let init_pop = List.nth init_pops id in
      let species_name = List.nth species_names id in
      let new_multigraph =
        init_multigraph [ init_pop ]
          ("Population of species " ^ species_name)
          timestep [ species_name ]
          (* generate different colors for each species *)
          (let dcolor = 1. /. (float_of_int n_species +. 1.) in
           let f_id = float_of_int id in
           [ (dcolor *. f_id, 1. -. (dcolor *. (f_id +. 1.)), dcolor *. f_id) ])
      in
      init_graph_lst (id + 1) (graph_lst @ [ (new_multigraph, id) ])
  in
  let multigraphs = init_graph_lst 0 [] in
  (* helper function to produce functions that add new points to the
     multigraphs *)
  let get_next_step_pop id : multigraph -> multigraph =
    let pop = ref 0 in
    fun mg ->
      pop := !pop + 1;
      let species_name = List.nth species_names id in
      let total_steps = List.length point_matrix - 1 in
      let next_step_pop =
        if !pop <= total_steps then
          List.nth (List.nth point_matrix !pop) id |> snd
        else List.nth (List.nth point_matrix total_steps) id |> snd
      in
      add_points_assoc [ (species_name, next_step_pop) ] mg
  in
  (* the list of pairs of multigraph and function *)
  let graph_fun_lst =
    List.map (fun (mg, id) -> (mg, get_next_step_pop id)) multigraphs
  in
  init_multigraphs graph_fun_lst
    ((find_duration m |> float_of_int) /. timestep |> int_of_float);
  launch ();
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\n\
     Thank you so much for using JQuirb evolution simutator engine.\n\
     Designed by: Justin Guo, Qifan Wang, Bryant Har.\n\n";
  exit 0

(** [eval_model m] calculates the populations for all species at the each
    timestep during the simulation of model [m], then it calls the next function
    to launch the graphs. If some computation in the process of generating the
    point matrix results in values exceeding the bounds for floats, then the
    simulator will ask the user to reenter the duration of simulation and step
    length. *)
let rec eval_model (m : Model.model) (timestep : float) : unit =
  ANSITerminal.print_string [ ANSITerminal.blue ] part_7_title;
  let num_step =
    float_of_int (find_duration m) /. timestep
    |> ceil |> int_of_float |> ( + ) 1
  in
  let point_matrix = graph_point m num_step 0 [] [] timestep in
  if point_matrix = [] then (
    ANSITerminal.print_string [ ANSITerminal.red ]
      "\n\
       The system breaks down. During the simulation, the population of some \
       species at some timestep exceed the bounds for floats.\n";
    ANSITerminal.print_string [ ANSITerminal.blue ] part_6_title;
    get_duration m)
  else launch_graphs m timestep point_matrix

(** [get_time_step m] asks the user to enter the length of time step of
    simulation for the model [m]. If the value is not a positive integer or
    float, an error message will be printed, and the user will re-enter the
    value. Otherwise, the simulator will enter the next step: evaluating the
    populations each time step of simulation for each species in the model. *)
and get_time_step (m : Model.model) : unit =
  print_endline
    "Please enter the time step of simulation for your model. Accepted values \
     are positive integers or positive floats only.";
  print_string "> ";
  try
    match read_line () |> float_of_string with
    | x ->
        if x > 0. then eval_model m x
        else (
          ANSITerminal.print_string [ ANSITerminal.red ]
            "The number you entered is not positive. \n";
          get_time_step m)
  with _ ->
    ANSITerminal.print_string [ ANSITerminal.red ]
      "The number you entered is invalid. Please enter a positive integer or \
       float for the time step of simulation for your model.\n";
    get_time_step m

(** [get_duration m] asks the user to enter the duration of simulation for the
    model [m]. If the value is not a positive integer, an error message will be
    printed, and the user will re-enter the value. Otherwise, the simulator will
    enter the next step: asking users to enter the length of time step for
    simulation of the model. *)
and get_duration (m : Model.model) : unit =
  print_endline
    "Please enter the duration of simulation for your model. Accepted values \
     are positive integers only.";
  print_string "> ";
  try
    match read_line () |> int_of_string with
    | x ->
        if x > 0 then get_time_step (add_duration x m)
        else (
          ANSITerminal.print_string [ ANSITerminal.red ]
            "The number you entered is not positive. \n";
          get_duration m)
  with _ ->
    ANSITerminal.print_string [ ANSITerminal.red ]
      "The number you entered is invalid. Please enter a positive integer for \
       the duration of simulation for your model.\n";
    get_duration m

(** [get_init_pop acc m] asks the user to enter the initial population of
    species of 0-based index [acc] in the model [m]. If the value is not an
    integer or float, an error message will be printed, and the user will
    re-enter the value. If [acc] is greater than or equal to the number of
    paramters in the model, the simulator will enter the next step: getting the
    duration of simulation for the model. *)
let rec get_init_pop (acc : int) (m : Model.model) : unit =
  let spec = species_names m in
  let num_spec = List.length spec in
  if acc < num_spec then (
    let this_spec = List.nth spec acc in
    Printf.printf
      "Please enter the value of the initial population of species %s. \n"
      this_spec;
    print_string "> ";
    try
      match read_line () |> float_of_string with
      | x -> get_init_pop (acc + 1) (add_init_pop this_spec x m)
    with _ ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "The number you entered is invalid. Please enter an integer or float \
         for the initial population of species\n";
      get_init_pop acc m)
  else ANSITerminal.print_string [ ANSITerminal.blue ] part_6_title;
  get_duration m

(** [get_par_val acc m] asks the user to enter the value of the parameter of
    0-based index [acc] in the model [m]. If the value is not an integer or
    float, an error message will be printed, and the user will re-enter the
    value. If [acc] is greater than or equal to the number of paramters in the
    model, the simulator will enter the next step: getting the initial
    populations for each species in the model. *)
let rec get_par_val (acc : int) (m : Model.model) : unit =
  let par = par_names m in
  let num_par = List.length par in
  if acc < num_par then (
    let this_par = List.nth par acc in
    Printf.printf "Please enter the value of parameter %s. \n" this_par;
    print_string "> ";
    try
      match read_line () |> float_of_string with
      | x -> get_par_val (acc + 1) (add_par_val this_par x m)
    with _ ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "The number you entered is invalid. Please enter an integer or float \
         for the value of the parameter\n";
      get_par_val acc m)
  else ANSITerminal.print_string [ ANSITerminal.blue ] part_5_title;
  get_init_pop 0 m

(** [get_diff_eqs pseudo_acc acc num_var m] asks the user to enter the
    differential equations of the species of 0-based index [acc] to the model.
    If the differential equation is inappropriate or invalid, an error message
    will be printed, and the user will re-enter the differential equation. If
    [acc] is greater than or equal to [num_var], the simulator will enter the
    next step: getting the values for the parameters of the model. *)
let rec get_diff_eqs (print_rules : bool) (acc : int) (num_var : int)
    (m : Model.model) : unit =
  if acc < num_var then begin
    let spec = List.nth (species_names m) acc in
    if print_rules = true then (
      print_endline
        "Now let's enter the differential equations. The differential \
         equations should be numbers or name of parameters or name of \
         variables concatenated by operators. You only need to type the right \
         side of the equation.\n";
      print_endline diff_eqs_rule_message)
    else ();
    Printf.printf "Please enter the differential equation of Species %i : %s.\n"
      (acc + 1) spec;
    print_string "> ";
    let s = read_line () in
    try
      match to_expression s with
      | exp ->
          if check_bound_values exp m = true then
            get_diff_eqs false (acc + 1) num_var (add_diff_eq spec exp m)
          else (
            ANSITerminal.print_string [ ANSITerminal.red ] unbound_name_message;
            get_diff_eqs false acc num_var m)
    with _ ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        invalid_diff_eq_name_message;
      check_review_rules acc num_var m
  end
  else ANSITerminal.print_string [ ANSITerminal.blue ] part_4_title;
  get_par_val 0 m

(** [check_review_rules acc num_var m] asks the user if they want to review the
    rules of valid differential equations when they entered an invalide
    differential equation in the iteration of 0-based index [acc], with the
    current model being [m]. The user can type either ["y"] or ["n"] to choose
    if they want to review the rules; if they type something else, they can only
    get to the next step if they type ["y"] or ["n"]. *)
and check_review_rules (acc : int) (num_var : int) (m : Model.model) : unit =
  print_endline review_rule_message;
  print_string "> ";
  let read = read_line () in
  if read = "y" then (
    print_endline diff_eqs_rule_message;
    get_diff_eqs false acc num_var m)
  else if read = "n" then get_diff_eqs false acc num_var m
  else ANSITerminal.print_string [ ANSITerminal.red ] invalid_choice_message;
  check_review_rules acc num_var m

(** [check_bound_values exp m] is [true] iff expression [exp] has all of its
    values bound in model [m]. That is, all the variables that appears in [exp]
    has to be either the name of a paramter or species in [m] or being
    indirectly bound via let expressions involving paramters or species in [m]. *)
and check_bound_values (exp : Ast.expr) (m : Model.model) : bool =
  match exp with
  | Var v ->
      if member_par v m = true || member_species v m = true then true
      else
        let msg =
          "The differential equation you entered includes an unbound name " ^ v
          ^ ". \n"
        in
        ANSITerminal.print_string [ ANSITerminal.red ] msg;
        false
  | Float _ -> true
  | Binop (_, exp1, exp2) ->
      check_bound_values exp1 m && check_bound_values exp2 m
  | Let (s, exp1, exp2) ->
      check_bound_values exp1 m && check_bound_values exp2 (add_par s m)
  | Deriv e | Sin e | Cos e | Tan e | Fact e -> check_bound_values e m

(** [get_parameters acc num_par m] asks the user to enter the name of parameters
    of 0-based index [acc] to the model. If the name is inappropriate or
    invalid, an error message will be printed, and the user will re-enter the
    name. If [acc] is greater than or equal to [num_par], the simulator will
    enter the next step: getting the differential equations of the model. *)
let rec get_parameters (acc : int) (num_par : int) (m : Model.model) : unit =
  if acc < num_par then begin
    if acc = 0 then
      Printf.printf
        "Please enter the name of Parameter %i. The name of a parameter should \
         be either a string of letters or a string of letters followed by s \
         string of numbers. Example: \"b\", \"alpha\", \"a11\"\n"
        (acc + 1)
    else Printf.printf "Please enter the name of Parameter %i.\n" (acc + 1);
    print_string "> ";
    let s = read_line () in
    try
      match to_expression s with
      | Var v ->
          if member_par v m = false then
            if member_species v m = false then
              get_parameters (acc + 1) num_par (add_par v m)
            else (
              ANSITerminal.print_string [ ANSITerminal.red ]
                duplicate_par_var_name_message;
              get_parameters acc num_par m)
          else (
            ANSITerminal.print_string [ ANSITerminal.red ]
              duplicate_par_name_message;
            get_parameters acc num_par m)
      | _ ->
          ANSITerminal.print_string [ ANSITerminal.red ]
            inappropriate_par_name_message;
          get_parameters acc num_par m
    with _ ->
      ANSITerminal.print_string [ ANSITerminal.red ] invalid_par_name_message;
      get_parameters acc num_par m
  end
  else
    let num_var = List.length (species_names m) in
    ANSITerminal.print_string [ ANSITerminal.blue ] part_3_title;
    get_diff_eqs true 0 num_var m

(** [get_num_par m] asks the user to enter the number of parameters to be
    included in the model [m]. *)
let rec get_num_par (m : model) : unit =
  print_string "> ";
  try
    match read_line () |> int_of_string with
    | x ->
        if x >= 0 then get_parameters 0 x m
        else
          ANSITerminal.print_string [ ANSITerminal.red ]
            "The number you entered is negative. Please enter a non-negative \
             integer for the number of parameters.\n";
        get_num_par m
  with _ ->
    ANSITerminal.print_string [ ANSITerminal.red ]
      "The number you entered is invalid. Please enter an integer for the \
       number of parameters.\n";
    get_num_par m

(** [get_variables acc num_var m] asks the user to enter the name of species of
    0-based index [acc] to the model. If the name is inappropriate or invalid,
    an error message will be printed, and the user will re-enter the name. If
    [acc] is greater than or equal to [num_var], the simulator will enter the
    next step: getting the paramters of the model. *)
let rec get_variables (acc : int) (num_var : int) (m : Model.model) : unit =
  if acc < num_var then begin
    if acc = 0 then
      Printf.printf
        "Please enter the name of Species %i. The name of a species should be \
         either a string of letters or a string of letters followed by s \
         string of numbers. Example: \"wolf\", \"Wolf1\", \"N12\"\n"
        (acc + 1)
    else Printf.printf "Please enter the name of Species %i.\n" (acc + 1);
    print_string "> ";
    let s = read_line () in
    try
      match to_expression s with
      | Var v ->
          if member_species v m = false then
            get_variables (acc + 1) num_var (add_species v m)
          else (
            ANSITerminal.print_string [ ANSITerminal.red ]
              duplicate_var_name_message;
            get_variables acc num_var m)
      | _ ->
          ANSITerminal.print_string [ ANSITerminal.red ]
            inappropriate_var_name_message;
          get_variables acc num_var m
    with _ ->
      ANSITerminal.print_string [ ANSITerminal.red ] invalid_var_name_message;
      get_variables acc num_var m
  end
  else ANSITerminal.print_string [ ANSITerminal.blue ] part_2_title;
  print_endline get_num_par_message;
  get_num_par m

(** [get_num_var ()] asks the user to enter the number of species to be included
    in the model. *)
let rec get_num_var () : unit =
  print_string "> ";
  try
    match read_line () |> int_of_string with
    | x ->
        if x > 0 then get_variables 0 x (init_model ())
        else
          ANSITerminal.print_string [ ANSITerminal.red ]
            "The number you entered is not positive. Please enter a positive \
             integer for the number of species.\n";
        get_num_var ()
  with _ ->
    ANSITerminal.print_string [ ANSITerminal.red ]
      "The number you entered is invalid. Please enter an integer for the \
       number of species.\n";
    get_num_var ()

(** [main ()] starts the evolution simulator. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\n\nWelcome to JQuirb evolution simutator engine.\n\n";
  ANSITerminal.print_string [ ANSITerminal.blue ] part_1_title;
  print_endline
    "Please enter the number of species you want to include in the ecosystem.";
  get_num_var ()

(* Execute the evolution simulator. *)
let () = main ()
