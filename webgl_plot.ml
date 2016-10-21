(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

open Js_windows
open Webgl_plot_misc
open Js_array
open FloatData

module Scene = Webgl_plot_scene
module Repere = Webgl_plot_repere
module Component = Webgl_plot_component
module Math = Webgl_plot_math
module Helper = Webgl_plot_dom_helper
module Export = Webgl_plot_export
module Histogram = Webgl_plot_histogram
module Surface = Webgl_plot_surface

type plot = {
  element: Element.t;
  user_state : Component.state;
  scene : Scene.scene;
  repere : Repere.t;
}

let default_export =
  let open Export in
  {
    x_axis = None;
    y_axis = None;
    z_axis = None;
    ratio = None;
    series = []
  }

let create ?(initial_value = default_export) () : plot =
  let {Export.x_axis; y_axis; z_axis; series; ratio} = initial_value in
  let renderer gl textbox_factory =
    let open Scene in
    let scene = prepare_scene gl textbox_factory in
    let repere = Repere.create scene in

    option_iter ratio (scene # set_ratio);

    option_iter x_axis (function {label; ticks; bounds} ->
        option_iter label (repere # set_x_axis_label);
        option_iter ticks (repere # set_x_axis_ticks);
        option_iter bounds (repere # set_x_axis_bounds));

    option_iter y_axis (function {label; ticks; bounds} ->
        option_iter label (repere # set_y_axis_label);
        option_iter ticks (repere # set_y_axis_ticks);
        option_iter bounds (repere # set_y_axis_bounds));

    option_iter z_axis (function {label; ticks; bounds} ->
        option_iter label (repere # set_z_axis_label);
        option_iter ticks (repere # set_z_axis_ticks);
        option_iter bounds (repere # set_z_axis_bounds));

    let update_min_max min_ref max_ref a d s =
      let n = Float32Array.length a in
      assert (n mod d = 0);
      for k = 0 to n / d - 1 do
        let x = Float32Array.get a ((d * k) + s) in
        if x < !min_ref then min_ref := x;
        if x > !max_ref then max_ref := x;
      done
    in

    let x_min = ref max_float in
    let x_max = ref min_float in
    let y_min = ref max_float in
    let y_max = ref min_float in
    let z_min = ref max_float in
    let z_max = ref min_float in

    List.iter (function
        | Export.Histogram Uniform {name; x; z; y; widths; depths; colors; border} ->
          let widths = option_map flatten_array_array widths in
          let depths = option_map flatten_array_array depths in
          let colors = option_map flatten_array_array_array colors in

          let x = float32_array x in
          let z = float32_array z in
          let y = flatten_array_array y in

          update_min_max x_min x_max x 1 0;
          update_min_max z_min z_max z 1 0;
          y_min := min !y_min 0.0;
          update_min_max y_min y_max y 1 0;

          ignore (Histogram.create scene ?widths ?colors ?depths ?name ?border (`Grid (x, z, y)))

        | Histogram List {name; centers; widths; depths; colors; border} ->
          let widths = option_map float32_array widths in
          let depths = option_map float32_array depths in
          let colors = option_map flatten_array_array colors in
          let centers = flatten_array_array centers in

          update_min_max x_min x_max centers 3 0;
          y_min := min !y_min 0.0;
          update_min_max y_min y_max centers 3 1;
          update_min_max z_min z_max centers 3 2;

          ignore (Histogram.create scene ?widths ?colors ?depths ?name ?border (`List centers))

        | Surface Uniform {name; x; z; y; colors; wireframe; alpha; magnetic} ->
          let colors = option_map flatten_array_array_array colors in
          let x = float32_array x in
          let z = float32_array z in
          let y = flatten_array_array y in

          update_min_max x_min x_max x 1 0;
          update_min_max z_min z_max z 1 0;
          update_min_max y_min y_max y 1 0;

          ignore (Surface.create scene ?colors ?wireframe ?name ?alpha ?magnetic ~parametric:false x z y)

        | Surface Parametric {name; a; b; p; colors; wireframe; alpha; magnetic} ->
          let colors = option_map flatten_array_array_array colors in
          let a = float32_array a in
          let b = float32_array b in
          let p = flatten_array_array_array p in

          update_min_max x_min x_max p 3 0;
          update_min_max y_min y_max p 3 1;
          update_min_max z_min z_max p 3 2;

          ignore (Surface.create scene ?colors ?wireframe ?name ?alpha ?magnetic ~parametric:true a b p)

        | _ -> (* TODO *) assert false) series;

    let x_min = !x_min in
    let x_max = !x_max in
    let y_min = !y_min in
    let y_max = !y_max in
    let z_min = !z_min in
    let z_max = !z_max in

    let automatic_bounds axis =
      match axis with Some { Export.bounds = Some _; _} -> false | _ -> true
    in

    if automatic_bounds x_axis then
      repere # set_x_axis_bounds (x_min, x_max);

    if automatic_bounds y_axis then
      repere # set_y_axis_bounds (y_min, y_max);

    if automatic_bounds z_axis then
      repere # set_z_axis_bounds (z_min, z_max);

    scene # set_frame {x_min; x_max; y_min; y_max; z_min; z_max};

    let automatic_ticks axis =
      match axis with Some { Export.ticks = Some _; _} -> false | _ -> true
    in
    let uniform_ticks ?(skip_first = false) n min max =
      let n = if n <= 3 then 3 else n in
      let d = if skip_first then 1 else 0 in
      let format = format_from_range (max -. min) in
      Array.init (n - d) (fun k ->
          let value = min +. (float (k + d)) *. (max -. min) /. (float (n - 1)) in
          { Export.value; label = format value })
      |> Array.to_list
    in

    if ratio = None then begin
      let x_range = x_max -. x_min in
      let y_range = y_max -. y_min in
      let z_range = z_max -. z_min in
      let m = max x_range (max y_range z_range) in
      scene # set_ratio (x_range /. m, y_range /. m, z_range /. m);
    end;

    let number_of_ticks ratio =
      int_of_float (15.0 *. ratio)
    in
    let x_ratio, y_ratio, z_ratio = scene # ratio in
    if automatic_ticks x_axis then
      repere # set_x_axis_ticks (uniform_ticks (number_of_ticks x_ratio) x_min x_max);
    if automatic_ticks y_axis then
      repere # set_y_axis_ticks (uniform_ticks ~skip_first:true (number_of_ticks y_ratio) y_min y_max);
    if automatic_ticks z_axis then
      repere # set_z_axis_ticks (uniform_ticks (number_of_ticks z_ratio) z_min z_max);

    (scene, repere), fun clock {Component.aspect; angle; move; pointer; width; height; _} ->
      scene # set_clock clock;

      scene # set_aspect aspect;
      scene # set_angle angle;
      scene # set_move move;
      scene # set_pointer pointer;

      scene # set_width (int_of_float width);
      scene # set_height (int_of_float height);
      scene # render
  in
  let element, user_state, (scene, repere) = Component.create_webgl_canvas renderer in
  { element; user_state; scene; repere}

let element {element; _} =
  element

let pointer_projection {scene; _} =
  scene # pointer_projection

let pointer_magnetic {scene; _} =
  scene # pointer_magnetic

let selected_object {scene; _} =
  match scene # selected with
  | Some obj -> Some (obj # name)
  | None -> None


let angle {user_state; _} = user_state.angle
let set_angle {user_state; _} angle =
  (* Note:scene's angle will be updated on next frame. *)
  user_state.angle <- angle

let move {user_state; _} = user_state.move
let set_move {user_state; _} move =
  (* Note:scene's move will be updated on next frame. *)
  user_state.move <- move

let set_x_axis_label {repere; _} label = repere # set_x_axis_label label

let set_y_axis_label {repere; _} label = repere # set_y_axis_label label

let set_z_axis_label {repere; _} label = repere # set_z_axis_label label

let set_x_axis_bounds {repere; scene; _} ((x_min, x_max) as bounds) =
  repere # set_x_axis_bounds bounds;
  scene # set_frame { scene # frame with x_min; x_max}

let set_y_axis_bounds {repere; scene; _} ((y_min, y_max) as bounds) =
  repere # set_y_axis_bounds bounds;
  scene # set_frame { scene # frame with y_min; y_max}

let set_z_axis_bounds {repere; scene; _} ((z_min, z_max) as bounds) =
  repere # set_z_axis_bounds bounds;
  scene # set_frame { scene # frame with z_min; z_max}

let set_x_axis_ticks {repere; _} ticks = repere # set_x_axis_ticks ticks
let set_y_axis_ticks {repere; _} ticks = repere # set_y_axis_ticks ticks
let set_z_axis_ticks {repere; _} ticks = repere # set_z_axis_ticks ticks


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
    append_to = (fun parent -> Element.append_child parent plot.element);
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
