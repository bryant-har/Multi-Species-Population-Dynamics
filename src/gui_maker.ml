open Cairo
open Guigraph

type graph_iterator = multigraph * (multigraph -> multigraph)

let win_width, win_height = (1200., 700.)
let width, height = (ref 0., ref 0.)
let multigraphs : graph_iterator ref list ref = ref []
let steps_left = ref ~-1
let graph_position_coord n = (n / 2, n mod 2)

let init_multigraphs mg steps =
  steps_left := steps;
  multigraphs := List.map (fun e -> ref e) mg

let shorten_float f : string =
  Float.round (f *. 100.) /. 100. |> string_of_float

let draw_horiz_axis cr y (x_off, y_off) label (c1, c2, c3, c4) =
  set_source_rgba cr c1 c2 c3 c4;
  move_to cr x_off (y +. y_off);
  line_to cr (!width +. x_off) (y +. y_off);
  stroke cr;
  let extents = text_extents cr label in
  move_to cr
    (x_off -. extents.width -. 5.)
    (y +. y_off +. (extents.height /. 2.));
  set_source_rgb cr 0. 0. 0.;
  show_text cr label

let draw_vert_axis cr x (x_off, y_off) label (c1, c2, c3, c4) =
  set_source_rgba cr c1 c2 c3 c4;
  move_to cr (x +. x_off) y_off;
  line_to cr (x +. x_off) (y_off +. !height);
  stroke cr;
  let extents = text_extents cr label in
  move_to cr
    (x +. x_off -. (extents.width /. 2.))
    (y_off +. !height +. extents.height +. 5.);
  set_source_rgb cr 0. 0. 0.;
  show_text cr label

let draw_axes (mg : multigraph) cr (x_off, y_off) =
  set_source_rgb cr 0. 0. 0.;
  let y1, y2 =
    ( get_graph_bounds (!width, !height) mg |> fst,
      get_graph_bounds (!width, !height) mg |> snd )
  in
  let x1, x2 = (0., !width) in
  let tlb, tub = t_bounds_multi mg in
  let lb, ub = y_bounds mg in
  if lb < 0. && ub > 0. then
    draw_horiz_axis cr
      (((y2 -. y1) *. (ub /. (ub -. lb))) +. y1)
      (x_off, y_off) "0" (0., 0., 0., 1.0)
  else ();
  let steps = [ 0.; 0.2; 0.4; 0.6; 0.8; 1. ] in
  set_source_rgba cr 0. 0. 0. 0.2;
  List.iter
    (fun y_ratio ->
      draw_horiz_axis cr
        (((y2 -. y1) *. y_ratio) +. y1)
        (x_off, y_off)
        (((ub -. lb) *. (1. -. y_ratio)) +. lb |> shorten_float)
        (0., 0., 0., 0.2))
    steps;
  List.iter
    (fun x_rat ->
      draw_vert_axis cr
        (((x2 -. x1) *. x_rat) +. x1)
        (x_off, y_off)
        (((tub -. tlb) *. x_rat) +. tlb |> shorten_float)
        (0., 0., 0., 0.2))
    steps;
  set_source_rgba cr 0. 0. 0. 1.;
  move_to cr x_off (y1 +. y_off);
  line_to cr x_off (y2 +. y_off);
  stroke cr

let draw_graph (g : graph) (mg : multigraph) cr (x_off, y_off) =
  let red, green, blue = color g in
  set_source_rgba cr 0. 0. 0. 1.;
  let gp = get_graph_points (!width, !height) g mg in
  move_to cr x_off (snd (List.nth gp 0) +. y_off);
  List.iter (fun (x, y) -> line_to cr (x +. x_off) (y +. y_off)) gp;
  set_source_rgb cr red green blue;
  let final_point_text =
    List.nth (get_points g) (List.length (get_points g) - 1) |> shorten_float
  in
  let extent = text_extents cr final_point_text in
  let fp_x, fp_y = List.nth gp (List.length gp - 1) in
  move_to cr (fp_x +. 3. +. x_off) (fp_y +. (extent.height /. 2.) +. y_off);
  set_font_size cr 8.;
  show_text cr final_point_text;
  stroke cr

let draw_multigraph (mg : multigraph) cr (x_off, y_off) =
  set_font_size cr 16.;
  let title = multi_name mg in
  let extent = text_extents cr title in
  move_to cr
    (x_off +. (!width /. 2.) -. (extent.width /. 2.))
    (y_off -. (extent.height /. 2.));
  show_text cr title;
  List.iter (fun g -> draw_graph g mg cr (x_off, y_off)) (graphs mg);
  draw_axes mg cr (x_off, y_off)

let draw_main cr =
  move_to cr 5. 15.;
  show_text cr
    (if !steps_left > 1 then "Simulating..." else "Simulation Concluded!");

  List.iteri
    (fun i (gi : graph_iterator ref) ->
      let x_p, y_p = graph_position_coord i in
      let mg = fst !gi in
      draw_multigraph mg cr
        ( 50. +. ((!width +. 50.) *. float_of_int x_p),
          50. +. ((!height +. 50.) *. float_of_int y_p) );
      gi := (mg |> snd !gi, snd !gi))
    !multigraphs

let looper d vbox () =
  if !steps_left > 0 then steps_left := !steps_left - 1 else ();
  if !steps_left = 0 then true
  else
    let _ = !d#destroy () in
    let new_d = GMisc.drawing_area ~packing:vbox#add () in
    ignore
      (new_d#misc#connect#draw ~callback:(fun cr ->
           draw_main cr;
           true));
    d := new_d;
    true

let launch () =
  ignore (GMain.init ());
  let num_graphs = List.length !multigraphs in
  width :=
    (win_width -. 50. -. (50. *. float_of_int ((1 + num_graphs) / 2)))
    /. float_of_int ((1 + num_graphs) / 2);
  height :=
    (win_height -. 50. -. (50. *. float_of_int (Stdlib.min 2 num_graphs)))
    /. float_of_int (Stdlib.min 2 num_graphs);
  let w =
    GWindow.window ~title:"Population Graphs"
      ~width:(win_width |> int_of_float)
      ~height:(win_height |> int_of_float)
      ()
  in
  ignore (w#connect#destroy ~callback:GMain.quit);
  let vbox = GPack.vbox ~packing:w#add () in
  let d = ref (GMisc.drawing_area ~packing:vbox#add ()) in
  Glib.Timeout.add ~ms:50 ~callback:(looper d vbox) |> ignore;
  w#show ();
  GMain.main ()
