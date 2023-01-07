type graph = {
  (*A points list [v1; v2; v3; ...; vn] is the list of points ordered from
    newest to oldest as vn, vn-1, ..., v1.*)
  points : float list;
  name : string;
  delta_t : float;
  color : float * float * float;
}

type multigraph = {
  graphs : graph list;
  name : string;
  delta_t : float;
}

let init_graph v st c n = { points = [ v ]; delta_t = st; name = n; color = c }
let empty_graph st c n = { points = []; delta_t = st; name = n; color = c }

let init_multigraph inits n st names colors =
  let graphs = ref [] in
  for i = 0 to List.length inits - 1 do
    let graph =
      init_graph (List.nth inits i) st (List.nth colors i) (List.nth names i)
    in
    graphs := graph :: !graphs
  done;
  graphs := List.rev !graphs;
  { graphs = !graphs; name = n; delta_t = st }

let name (g : graph) = g.name
let step (g : graph) = g.delta_t
let num_points (g : graph) = List.length g.points
let graphs (mg : multigraph) = mg.graphs
let points (g : graph) = List.rev g.points

let num_points_multi (mg : multigraph) =
  List.fold_left (fun acc e -> Stdlib.max (num_points e) acc) 0 mg.graphs

let color (g : graph) = g.color
let multi_name (mg : multigraph) = mg.name

let min g =
  List.fold_left
    (fun acc e -> if e < acc then e else acc)
    (List.nth g.points 0) g.points

let max g =
  List.fold_left
    (fun acc e -> if e > acc then e else acc)
    (List.nth g.points 0) g.points

let min_multi (mg : multigraph) =
  List.fold_left
    (fun acc (e : graph) -> if min e < acc then min e else acc)
    (List.nth mg.graphs 0 |> min)
    mg.graphs

let max_multi (mg : multigraph) =
  List.fold_left
    (fun acc (e : graph) -> if max e > acc then max e else acc)
    (List.nth mg.graphs 0 |> max)
    mg.graphs

let t_bounds g =
  if List.length g.points > 5 then
    (0., 1.1 *. g.delta_t *. float_of_int (List.length g.points))
  else (0., g.delta_t *. 5.)

let t_bounds_multi mg =
  List.fold_left
    (fun acc e ->
      let low, high = t_bounds e in
      (Stdlib.min (fst acc) low, Stdlib.max (snd acc) high))
    (t_bounds (List.nth mg.graphs 0))
    mg.graphs

let y_bounds mg =
  let highest = max_multi mg in
  let lowest = min_multi mg in
  if highest <> lowest then
    ( lowest -. ((highest -. lowest) *. 0.1),
      highest +. ((highest -. lowest) *. 0.1) )
  else (lowest -. 0.1, highest +. 0.1)

let add_point v g = { g with points = v :: g.points }

let add_points vs mg =
  {
    mg with
    graphs = List.mapi (fun i g -> add_point (List.nth vs i) g) mg.graphs;
  }

let add_points_assoc assoc mg =
  {
    mg with
    graphs =
      List.map (fun g -> add_point (List.assoc (name g) assoc) g) mg.graphs;
  }

let get_points g = List.rev g.points

let get_relative_point (p : float) (mg : multigraph) =
  let low, high = y_bounds mg in
  (p -. low) /. (high -. low)

let get_relative_points (g : graph) (mg : multigraph) =
  List.map (fun p -> get_relative_point p mg) (List.rev g.points)

let rec graph_points start_x x_step = function
  | [] -> []
  | (h : float) :: t ->
      (start_x, h) :: graph_points (start_x +. x_step) x_step t

let get_graph_points ((w : float), (h : float)) (g : graph) (mg : multigraph) =
  let t1, t2 = t_bounds_multi mg in
  let x_step =
    mg.delta_t /. (t2 -. t1) *. w
    *. ((num_points_multi mg |> ( + ) 1 |> float_of_int)
       /. (num_points_multi mg |> float_of_int))
  in
  graph_points t1 x_step
    (List.map (fun y -> h -. (y *. h)) (get_relative_points g mg))

let get_graph_bounds (w, (h : float)) mg =
  ignore mg;
  ignore w;
  (0., h)
