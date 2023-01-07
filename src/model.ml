open Expression
open Eval

type parameter = {
  par : string;
  value : float;
}

type species = {
  name : string;
  init_pop : float;
  diff_eq : Ast.expr;
}

type model = {
  species_lst : species list;
  parameters : parameter list;
  duration : int;
}

exception DuplicateSpecies
exception DuplicatePar
exception UnfoundSpecies
exception UnfoundPar
exception InvalidDuration

let init_model () = { species_lst = []; parameters = []; duration = 0 }

let species_names (m : model) : string list =
  List.map (fun elt -> elt.name) m.species_lst

let par_names (m : model) : string list =
  List.map (fun elt -> elt.par) m.parameters

(** [find_species s m] is the species with name [s] in model [m]. Raises:
    [UnfoundSpecies] if there is no species in model [m] named [s]. Raises:
    [DuplicateSpecies] if there are more than one species in model [m] named
    [s]. *)
let find_species (s : string) (m : model) : species =
  match List.filter (fun x -> x.name = s) m.species_lst with
  | [] -> raise UnfoundSpecies
  | [ h ] -> h
  | _ -> raise DuplicateSpecies
(* the last branch will never be matched if other functions are implemented
   correctly *)

let member_species (s : string) (m : model) : bool =
  match List.filter (fun x -> x.name = s) m.species_lst with
  | [] -> false
  | _ -> true

(** [find_par s m] is the parameter with name [s] in model [m]. Raises:
    [UnfoundPar] if there is no parameter in model [m] named [s]. Raises:
    [DuplicatePar] if there are more than one parameters in model [m] named [s].*)
let find_par (s : string) (m : model) : parameter =
  match List.filter (fun x -> x.par = s) m.parameters with
  | [] -> raise UnfoundPar
  | [ h ] -> h
  | _ -> raise DuplicatePar
(* the last branch will never be matched if other functions are implemented
   correctly *)

let member_par (s : string) (m : model) : bool =
  match List.filter (fun x -> x.par = s) m.parameters with
  | [] -> false
  | _ -> true

(** [get_elts_before_elt f lst] is the list elements in the list [lst] that
    appear before the first element [elt] in [lst] such that [f elt] is [true].
    If [lst] is empty, then [get_elts_before_elt f lst] is [\[\]]. If [lst] is
    non-empty and for all elements [elt] in [lst], [f elt] is [false], then
    [get_elts_before_elt f lst] is [lst]. *)
let rec get_elts_before_elt (f : 'a -> bool) (lst : 'a list) : 'a list =
  match lst with
  | [] -> []
  | h :: t -> if f h then [] else h :: get_elts_before_elt f t

(** [get_elts_after_elt f lst] is the list elements in the list [lst] that
    appear after the first element [elt] in [lst] such that [f elt] is [true].
    If [lst] is empty, or if [lst] is non-empty and for all elements [elt] in
    [lst], [f elt] is [false], then [get_elts_before_elt f lst] is [\[\]]. *)
let rec get_elts_after_elt (f : 'a -> bool) (lst : 'a list) : 'a list =
  match lst with
  | [] -> []
  | h :: t -> if f h then t else get_elts_after_elt f t

let find_init_pop (s : string) (m : model) : float = (find_species s m).init_pop

let find_diff_eq (s : string) (m : model) : Ast.expr =
  (find_species s m).diff_eq

let find_par_val (s : string) (m : model) : float = (find_par s m).value
let find_duration (m : model) : int = m.duration

let add_species (s : string) (m : model) : model =
  let init_species_lst = m.species_lst in
  if List.filter (fun x -> x.name = s) init_species_lst = [] then
    let new_species = { name = s; init_pop = 0.; diff_eq = Float 0. } in
    { m with species_lst = init_species_lst @ [ new_species ] }
  else raise DuplicateSpecies

let add_diff_eq (s : string) (eq : Ast.expr) (m : model) : model =
  let init_species_lst = m.species_lst in
  let match_name x = x.name = s in
  let before_species = get_elts_before_elt match_name init_species_lst in
  let after_species = get_elts_after_elt match_name init_species_lst in
  let species_s = find_species s m in
  let updated_species_s = { species_s with diff_eq = eq } in
  {
    m with
    species_lst = before_species @ [ updated_species_s ] @ after_species;
  }

let add_init_pop (s : string) (p : float) (m : model) : model =
  let init_species_lst = m.species_lst in
  let match_name x = x.name = s in
  let before_species = get_elts_before_elt match_name init_species_lst in
  let after_species = get_elts_after_elt match_name init_species_lst in
  let species_s = find_species s m in
  let updated_species_s = { species_s with init_pop = p } in
  {
    m with
    species_lst = before_species @ [ updated_species_s ] @ after_species;
  }

let add_duration (d : int) (m : model) : model =
  if d >= 0 then { m with duration = d } else raise InvalidDuration

let add_par (s : string) (m : model) : model =
  let init_par_lst = m.parameters in
  if List.filter (fun x -> x.par = s) init_par_lst = [] then
    let new_par = { par = s; value = 0. } in
    { m with parameters = init_par_lst @ [ new_par ] }
  else raise DuplicatePar

let add_par_val (s : string) (v : float) (m : model) : model =
  let init_parameters = m.parameters in
  let match_name x = x.par = s in
  let before_pars = get_elts_before_elt match_name init_parameters in
  let after_pars = get_elts_after_elt match_name init_parameters in
  let par_s = find_par s m in
  let updated_par_s = { par_s with value = v } in
  { m with parameters = before_pars @ [ updated_par_s ] @ after_pars }

(** [find_id elt lst] is the 0-based index of element [elt] in list [lst]. If
    [elt] is not an element in [lst], failwith "the element is not found in the
    list". *)
let rec find_id (elt : 'a) (lst : 'a list) : int =
  match lst with
  | [] -> failwith "the element is not found in the list"
  | h :: t -> if h = elt then 0 else 1 + find_id elt t

(** [graph_point_step_single sp prev step m] is the point [(x, y)] on the graph
    representing the population of species [sp] in model [m] at one time step
    [step] after the time in [prev]. [x] is the current time, and [y] is the
    current population size of species [sp]. *)
let graph_point_step_single (sp : string) (prev_step : (float * float) list)
    (time_step : float) (m : model) : float * float =
  let spec_lst = species_names m in
  let prev_point = List.nth prev_step (find_id sp spec_lst) in
  let expr = find_diff_eq sp m |> convert in
  let par_lst = par_names m in
  let par_bindings =
    List.map
      (fun par ->
        (par, find_par_val par m |> string_of_float |> to_expression |> convert))
      par_lst
  in
  let spec_bindings =
    List.map
      (fun spec ->
        let pop = snd (List.nth prev_step (find_id spec spec_lst)) in
        (spec, pop |> string_of_float |> to_expression |> convert))
      spec_lst
  in
  let bindings = par_bindings @ spec_bindings in
  regenerateEuler prev_point time_step bindings expr

let rec graph_point (m : model) (num_step : int) (acc : int)
    (prev_step : (float * float) list) (prev : (float * float) list list)
    (time_step : float) : (float * float) list list =
  if num_step < 0 then failwith "the number of steps is negative"
  else if acc < 0 then
    failwith "the number of steps that have been gone through is negative"
  else if num_step < acc then
    failwith
      "the number of steps is less than the number of steps that have been \
       gone through"
  else
    try
      let spec_lst = species_names m in
      if num_step = 0 then
        [ List.map (fun sp -> (0., find_init_pop sp m)) spec_lst ]
      else if acc < num_step then
        let new_step =
          if acc = 0 then List.map (fun sp -> (0., find_init_pop sp m)) spec_lst
          else
            List.map
              (fun sp -> graph_point_step_single sp prev_step time_step m)
              spec_lst
        in
        graph_point m num_step (acc + 1) new_step (prev @ [ new_step ])
          time_step
      else prev
    with _ -> []
