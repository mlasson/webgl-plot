open Js_windows

module Export = Webgl_plot_export

open Webgl_plot

type js_histogram =
  {
    set_alpha: (float option -> unit);
    set_border: (float -> unit);
  } [@@js]

let js_histogram h =
  let open Histogram in
  {
    set_alpha = set_alpha h;
    set_border = set_border h;
  }

type js_surface =
  {
    set_alpha: (float option -> unit);
    set_wireframe: (bool -> unit);
    set_magnetic: (bool -> unit);
    x_projection: (float -> (float array * float array) option);
    z_projection: (float -> (float array * float array) option);
  } [@@js]

let js_surface s =
  let open Surface in
  {
    set_alpha = set_alpha s;
    set_wireframe = set_wireframe s;
    set_magnetic = set_magnetic s;
    x_projection = x_projection s;
    z_projection = z_projection s;
  }

type js_interface = {
  append_to: Element.t -> unit;

  add_histogram: (Export.Histogram.t -> js_histogram);
  add_surface: (Export.Surface.t -> js_surface);

  get_element: (unit -> Element.t);
  get_pointer_projection: (unit -> float * float * float);
  get_pointer_magnetic: (unit -> float * float * float);
  get_selected_object: (unit -> string option);

  set_on_double_click: ((unit -> unit) -> unit);
  set_pointer_text_formatter: ((Js_windows.Element.t -> unit) -> unit);
  update_pre_render_hook: (((unit -> unit) -> unit -> unit) -> unit);
  update_post_render_hook: (((unit -> unit) -> unit -> unit) -> unit);

  set_x_axis_label: (string -> unit);
  set_y_axis_label: (string -> unit);
  set_z_axis_label: (string -> unit);

  set_x_axis_ticks: (Export.tick list -> unit);
  set_y_axis_ticks: (Export.tick list -> unit);
  set_z_axis_ticks: (Export.tick list -> unit);

  set_x_axis_bounds: (float * float -> unit);
  set_y_axis_bounds: (float * float -> unit);
  set_z_axis_bounds: (float * float -> unit);

  get_angle: (unit -> float * float * float);
  set_angle: (float * float * float -> unit);
  get_move: (unit -> float * float * float);
  set_move: (float * float * float -> unit);
} [@@js]

let js_interface initial_value =
  let plot = create ?initial_value () in
  {
    append_to = (fun parent -> Element.append_child parent (element plot));

    add_histogram = Histogram.(function
        | Export.Histogram.Grid {name; x; z; y; border; widths; depths; colors} ->
          js_histogram (add_grid_histogram plot ?name ?border ?widths ?depths ?colors ~x ~z ~y ())
        | Export.Histogram.List {name; centers; border; widths; depths; colors} ->
          js_histogram (add_list_histogram plot ?name ?border ?widths ?depths ?colors centers)
        | _ -> assert false (* TODO *));

    add_surface = Surface.(function
        | Export.Surface.Graph {name; x; z; y; colors; alpha; wireframe; magnetic} ->
          js_surface (add_surface plot ?name ?colors ?alpha ?wireframe ?magnetic ~x ~z ~y ())
        | Export.Surface.Parametric {name;a; b; p; colors; alpha; wireframe; magnetic} ->
          js_surface (add_parametric_surface plot ?name ?colors ?alpha ?wireframe ?magnetic ~a ~b ~p ())
        | _ -> assert false (* TODO *));

    get_element = (fun () -> element plot);
    get_pointer_projection = (fun () -> pointer_projection plot);
    get_pointer_magnetic = (fun () -> pointer_magnetic plot);
    get_selected_object = (fun () -> selected_object plot);

    set_on_double_click = on_double_click plot;
    set_pointer_text_formatter = set_pointer_text_formatter plot;
    update_pre_render_hook = update_pre_render_hook plot;
    update_post_render_hook = update_post_render_hook plot;

    set_x_axis_label = set_x_axis_label plot;
    set_y_axis_label = set_y_axis_label plot;
    set_z_axis_label = set_z_axis_label plot;

    set_x_axis_ticks = set_x_axis_ticks plot;
    set_y_axis_ticks = set_y_axis_ticks plot;
    set_z_axis_ticks = set_z_axis_ticks plot;

    set_x_axis_bounds = set_x_axis_bounds plot;
    set_y_axis_bounds = set_y_axis_bounds plot;
    set_z_axis_bounds = set_z_axis_bounds plot;

    get_angle = (fun () -> angle plot);
    set_angle = set_angle plot;
    get_move = (fun () -> move plot);
    set_move = set_move plot;
  }

let () =
  let o = Ojs.empty_obj () in
  Ojs.set Ojs.global "WebglPlot" o;
  Ojs.set o "create" ([%js.of: Export.chart option -> js_interface] js_interface)
