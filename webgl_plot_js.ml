open Js_browser

module Export = Webgl_plot_export

open Webgl_plot

type js_histogram =
  {
    id: int;
    name: string;
    set_alpha: (float option -> unit);
    set_border: (float -> unit);
  } [@@js]

let js_histogram h =
  let open Histogram in
  {
    id = id h;
    name = name h;
    set_alpha = set_alpha h;
    set_border = set_border h;
  }

type js_surface =
  {
    id: int;
    name: string;
    set_alpha: (float option -> unit);
    set_wireframe: (bool -> unit);
    set_magnetic: (bool -> unit);
    set_crosshair: (bool -> unit);
    x_projection: (float -> (float * float) list);
    y_projection: (float -> (float * float) list);
    z_projection: (float -> (float * float) list);
  } [@@js]

let js_surface s =
  let open Surface in
  {
    id = id s;
    name = name s;
    set_alpha = set_alpha s;
    set_wireframe = set_wireframe s;
    set_magnetic = set_magnetic s;
    set_crosshair = set_crosshair s;
    x_projection = x_projection s;
    y_projection = y_projection s;
    z_projection = z_projection s;
  }

type js_interface = {
  append_to: Element.t -> unit;

  add_histogram: (Export.Histogram.t -> js_histogram);
  add_surface: (Export.Surface.t -> js_surface);

  get_element: (unit -> Element.t);

  get_object: (int -> Ojs.t option);
  get_from_name: (string -> Ojs.t list);
  get_pointer_projection: (unit -> float * float * float);
  get_pointer_magnetic: (unit -> float * float * float);
  get_selected_object: (unit -> Ojs.t option);

  set_on_double_click: ((unit -> unit) -> unit);
  set_pointer_text_formatter: ((Js_browser.Element.t -> unit) -> unit);
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

  remove: (int -> unit);
  screenshot: ((string -> unit) -> unit);
} [@@js]

let js_interface initial_value =
  let plot = create ?initial_value () in
  let get_object id =
    match Histogram.get plot id with
    | Some h -> Some ([%js.of: js_histogram] (js_histogram h))
    | None -> match Surface.get plot id with
      | Some s -> Some ([%js.of: js_surface] (js_surface s))
      | None -> None;
  in
  {
    append_to = (fun parent -> Element.append_child parent (element plot));

    add_histogram = Histogram.(function
        | Export.Histogram.Grid {name; x; z; y; border; widths; depths; floors; colors} ->
          js_histogram (add_grid_histogram plot ?name ?border ?widths ?depths ?floors ?colors ~x ~z ~y ())
        | Export.Histogram.List {name; centers; border; widths; depths; floors; colors} ->
          js_histogram (add_list_histogram plot ?name ?border ?widths ?depths ?floors ?colors centers)
        | _ -> assert false (* TODO *));

    add_surface = Surface.(function
        | Export.Surface.Grid {name; centers; colors; alpha; wireframe; magnetic; crosshair} ->
          js_surface (add_surface plot ?name ?colors ?alpha ?wireframe ?magnetic ?crosshair centers)
        | _ -> assert false (* TODO *));

    get_element = (fun () -> element plot);

    get_object;
    get_from_name = (fun name ->
        ((Histogram.get_from_name plot name) |> List.map (fun h -> [%js.of: js_histogram] (js_histogram h)))
          @
        ((Surface.get_from_name plot name) |> List.map (fun s -> [%js.of: js_surface] (js_surface s)))
      );
    get_pointer_projection = (fun () -> pointer_projection plot);
    get_pointer_magnetic = (fun () -> pointer_magnetic plot);
    get_selected_object = (fun () ->
        match selected_object plot with
        | None -> None
        | Some id -> get_object id);

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

    remove = remove plot;
    screenshot = screenshot plot;
  }

let () =
  let o = Ojs.empty_obj () in
  Ojs.set Ojs.global "WebglPlot" o;
  Ojs.set o "create" ([%js.of: Export.chart option -> js_interface] js_interface)
