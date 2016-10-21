open Js_windows

module Export = Webgl_plot_export

open Webgl_plot

type javascript_interface = {
  append_to: Element.t -> unit;
  get_element: (unit -> Element.t);
  get_pointer_projection: (unit -> float * float * float);
  get_pointer_magnetic: (unit -> float * float * float);
  get_selected_object: (unit -> string option);

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

let javascript_interface initial_value =
  let plot = create ?initial_value () in
  {
    append_to = (fun parent -> Element.append_child parent (element plot));
    get_element = (fun () -> element plot);
    get_pointer_projection = (fun () -> pointer_projection plot);
    get_pointer_magnetic = (fun () -> pointer_magnetic plot);
    get_selected_object = (fun () -> selected_object plot);

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
  Ojs.set o "create" ([%js.of: Export.chart option -> javascript_interface] javascript_interface)
