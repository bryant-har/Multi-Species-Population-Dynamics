(** Functions for initializing and launching the GUI graph component. *)

open Guigraph

type graph_iterator = multigraph * (multigraph -> multigraph)
(** A graph iterator is a tuple containing a current multigraph paired with a
    function that takes in the multigraph and changes it on its next iteration.*)

val init_multigraphs : graph_iterator list -> int -> unit
(** [init_multigraphs \[(mg1, f1); (mg2, f2); ...; (mgn, fn)\]] sets up the
    multigraphs [mg1], [mg2], ..., and [mgn] with respective functions [f1],
    [f2], ..., [fn] that each step the multigraph by one iteration.*)

val launch : unit -> unit
(**[launch ()] opens the window and displays the multigraphs initialized by
   [init_multigraphs].*)