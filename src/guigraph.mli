(** Graph abstract types / objects that allow the creation of graphs, along with
    helper functions useful for drawing said graph in the future. *)

type graph
(** The abstract type of values representing a graph. *)

type multigraph
(** The abstract type of values representing a multigraph. *)

val init_graph : float -> float -> float * float * float -> string -> graph
(** [init_graph v st c n] is a graph with its starting value as [v], a stepping
    delta t of [st], color of [c] and name of [n].*)

val empty_graph : float -> float * float * float -> string -> graph
(** [empty_graph st c n] is a graph with no points, a stepping delta t of [st],
    color of [c] and name of [n].*)

val init_multigraph :
  float list ->
  string ->
  float ->
  string list ->
  (float * float * float) list ->
  multigraph
(** [init_multigraph inits n st names colors] is a multigraph whose internal
    graphs have initial values [inits], names [names], delta_t's all equal to
    st, and colors [colors]. The multigraph itself will have name [n].*)

val name : graph -> string
(** [graph_name g} is the name of the graph [g]. *)

val multi_name : multigraph -> string
(** [multi_name mg] is the name of the multigraph [mg].*)

val step : graph -> float
(** [step g] is the stepping delta t of graph [g].*)

val graphs : multigraph -> graph list
(** [graphs mg] is the list of graphs in [mg]. *)

val num_points : graph -> int
(** [num_points g] is the number of points in the graph [g].*)

val points : graph -> float list
(** [points g] is a list of the points in the graph [g] sorted from oldest to
    newest.*)

val color : graph -> float * float * float
(** [color g] is a three-float tuple detailing the color of graph [g].*)

val min : graph -> float
(** [min g] is the value of the point with the lowest value in [g].*)

val max : graph -> float
(** [max g] is the value of the point with the highest value in [g].*)

val min_multi : multigraph -> float
(** [min_multi mg] is the value of the point with the lowest value in all the
    graphs in [mg].*)

val max_multi : multigraph -> float
(** [max_multi mg] is the value of the point with the highest value in all the
    graphs in [mg].*)

val t_bounds : graph -> float * float
(** [t_bounds g] is a tuple [(a, b)] containing the lower and upper t bounds,
    [a] and [b] respectively, to be displayed for the graph.*)

val t_bounds_multi : multigraph -> float * float
(** [t_bounds_multi mg] is a tuple [(a, b)] containing the lower and upper t
    bounds, [a] and [b] respectively, to be displayed for the mutligraph.*)

val y_bounds : multigraph -> float * float
(** [y_bounds mg] is a tuple [(a, b)] containing the lower and upper y bounds,
    [a] and [b] respectively, to be displayed for the graph [mg].*)

val add_point : float -> graph -> graph
(** [add_point v g] is the new graph created by adding a point [v] as the newest
    point to graph [g].*)

val add_points : float list -> multigraph -> multigraph
(** [add_points vs mg] is the new multigraph created by adding points [vs] as
    the newest points to the graphs in [mg].

    Requires: The length of [vs] is equal to the number of graphs in mg.*)

val add_points_assoc : (string * float) list -> multigraph -> multigraph
(** [add_points_assoc assoc mg] is the new multigraph created by adding points
    in [assoc], formatted as an association list of the population name to the
    new point, as the newest points to the graphs in [mg].

    Requires: [assoc] is an assocation list that contains bindings for all
    graphs in [mg].*)

val get_points : graph -> float list
(** [get_points g] is a list of points sorted from oldest to newest
    corresponding to the points of graph [g].*)

val get_relative_point : float -> multigraph -> float
(** [get_relative_point p mg] is point [p] as its relative position within 0..1
    corresponding to the ratio that the graph point [p] should be placed at
    vertically.

    Requires: p is a point in [mg].*)

val get_relative_points : graph -> multigraph -> float list
(** [get_relative_point g mg] is the list of points sorted from oldest to newest
    of [g] within 0..1 corresponding to the ratio that the graph point should be
    placed at vertically within [mg]. *)

val get_graph_points :
  float * float -> graph -> multigraph -> (float * float) list
(** [get_graph_points (w, h) g mg] is the list of lists of points sorted from
    leftmost to rightmost of [g] corresponding to the actual pixel coordinates
    in the drawn graph that each point in [mg] should be drawn at according to
    dimensions [w] and [h].*)

val get_graph_bounds : float * float -> multigraph -> float * float
(** [get_graph_bounds (w, h) mg] is a tuple [(y1, y2)] consisting of the lower
    and upper bounds of the visual graph of [mg] of width and height [w] and
    [h].*)
