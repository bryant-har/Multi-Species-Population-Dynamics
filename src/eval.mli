(** Evaluates parsed expressions into useful values. *)

type expr
(** The abstract type of values representing the expression. *)

val convert : Ast.expr -> expr
(** [convert e] is the expression [e] being converted to type [expr]. *)

val convert_back : expr -> Ast.expr
(** [convert_back e] is the expression [e] being converted to type [Ast.expr]. *)

val fact : int -> int -> int
(** [fact acc i] is [acc * i!], where [i!] is the factorial of [i]. If [i] is
    negative, then fail with ["Negative Argument"]. *)

val sub : expr -> expr
(**[sub e] is the new expression obtained by making any valid let substitutions
   in old expression [e]. *)

val forceSub : string * expr -> expr -> expr
(**[forceSub (var, value) e] is the new expression that is optained by
   substituting [var] with [value] in the old expression [e], and then making
   any valid let substitutions. *)

val fullEval : expr -> expr
(**[fullEval e] is the expression that is obtained by simplifying any possible
   computations in the old expression [e]. Requires: [e] should contain no
   derivatives, and no undetermined variables exist. *)

val regenerateEuler :
  float * float -> float -> (string * expr) list -> expr -> float * float
(** [regenerateEuler (x,y) timestep assoclist e] is the point [(x_new, y_new)]
    representing the population of a species at the next timesetp given [x],
    which is the current time, [y], which is the current population of the
    species, [timestep], which is the length of a time step (dt), [assoclist],
    which are the bindings of expressions to variables, and [e], which is the
    differential equation for the species.

    Example: if a population [S] at time [1.] has population [2.], [(x, y)] is
    [(1., 2.)]. If it is represented by some diff eq [dS/dt = 0.5 *. S +. a],
    [e] is [0.5 *. S +. a]. Suppose [a] has value [1.], then [assoclist] is
    [\[("a", 1.)\]]. If [timestep = 1.], the point generated will be
    [(1. + 1. , 2. +.(1. * (0.5 *. 2. +. 1.))) = (2., 4.)]. *)
