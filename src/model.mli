(** A model containing all parameters, species, and equations for a simulation.*)

type model
(** The abstract type of values representing the model. *)

exception DuplicateSpecies
(** Raised when there are more than one species of the same name in the model. *)

exception DuplicatePar
(** Raised when there are more than one parameters of the same name in the
    model. *)

exception UnfoundSpecies
(** Raised when the species does not exist in the model. *)

exception UnfoundPar
(** Raised when the parameter does not exist in the model. *)

exception InvalidDuration
(** Raised when the duration of simulation for the model is invalid. *)

val init_model : unit -> model
(** [init_model ()] initializes a model for the simulation. *)

val species_names : model -> string list
(** [species_names m] is the list of names of all species in model [m]. *)

val member_species : string -> model -> bool
(** [member_species s m] is [true] iff [s] is the name of an existent species in
    model [m]. *)

val par_names : model -> string list
(** [par_names m] is the list of names of all parameters in model [m]. *)

val member_par : string -> model -> bool
(** [member_par s m] is [true] iff [s] is the name of an existent parameter in
    model [m]. *)

val find_init_pop : string -> model -> float
(** [find_init_pop s m] is the initial population of the species with name [s]
    in model [m]. If [s] is not the name of any existant species in the model
    [m], raises [UnfoundSpecies]. *)

val find_diff_eq : string -> model -> Ast.expr
(** [find_diff_eq s m] is the diff eq of the species with name [s] in model [m].
    If [s] is not the name of any existant species in the model [m], raises
    [UnfoundSpecies]. *)

val find_duration : model -> int
(** [find_duration m] is the duration of the simulation in model [m]. *)

val find_par_val : string -> model -> float
(** [find_par_val s m] is the value of the parameter with name [s] in model [m].
    If [s] is not the name of any parameter in the model [m], raises
    [UnfoundPar]. *)

val add_species : string -> model -> model
(** [add_species s m] is model [m] plus one more species with name [s], init_pop
    [0.], and diff_eq [Float 0.] if [s] is not the name of any existant species
    in the model [m]. Raises [DuplciateSpecies] if [s] is already in model [m]. *)

val add_diff_eq : string -> Ast.expr -> model -> model
(** [add_diff_eq s eq m] is model [m] with the diff eq of species named [s]
    being updated with [eq]. Raises [UnfoundSpecies] if [s] is not the name of
    any existant species in the model [m]. *)

val add_init_pop : string -> float -> model -> model
(** [add_initi_pop s p m] is model [m] with the population of species named [s]
    being updated with [p]. Raises [UnfoundSpecies] if [s] is not the name of
    any existant species in the model [m]. *)

val add_duration : int -> model -> model
(** [add_duration d m] is the model [m] with the field duration being updated
    with [d]. Raises [InvalidDuration] if [d] is negative. *)

val add_par : string -> model -> model
(** [add_par s m] is model [m] plus one more par with names [s] and value [0.]
    if [s] is not the name of any existant parameters in the model [m]. Raises
    [DuplciatePar] if [s] is already in model [m]. *)

val add_par_val : string -> float -> model -> model
(** [add_par_val s v m] is model [m] with the value of parameter named [s] being
    updated with [v]. Raises [UnfoundPar] if [s] is not the name of any existant
    parameters in the model [m]. *)

val graph_point :
  model ->
  int ->
  int ->
  (float * float) list ->
  (float * float) list list ->
  float ->
  (float * float) list list
(** [graph_point m num_step acc prev_step prev step] is a list of lists of
    points on the graph representing the populations for all the species in
    model [m] from time [0] to time [max{0, (num_step - 1)} * step] given the
    length of time step [step]. The list of lists of points is constructed from
    appending the list of points [new_step] representing the populations at time
    [acc * step] to the list of lists of points [prev], where [new_step] is
    constructed from model [m], time step [step], and the list of points at the
    previous time step [prev_step]. If [num_step] or [acc] is negative, or if
    [num_step] is less then [acc], a failure will be raised. If some computation
    in the process results in values exceeding the bound for floats, then the
    value of the output is [\[\]]. *)
