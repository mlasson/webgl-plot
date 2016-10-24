(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

open Js_windows
open Webgl_plot_misc
open FloatData

module Scene = Webgl_plot_scene
module Repere = Webgl_plot_repere
module Component = Webgl_plot_component
module Math = Webgl_plot_math
module Helper = Webgl_plot_dom_helper
module Export = Webgl_plot_export

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


module Histogram =
struct
  module Histogram = Webgl_plot_histogram

  type t = Histogram.t

  let set_alpha histogram x = histogram # set_alpha x
  let set_border histogram x = histogram # set_border x

  let create_grid_histogram scene ?name ?border ?widths ?depths ?colors ~x ~z ~y () =
    let widths = option_map flatten_array_array widths in
    let depths = option_map flatten_array_array depths in
    let colors = option_map flatten_triple_array_array colors in
    let x = float32_array x in
    let z = float32_array z in
    let y = flatten_array_array y in
    Histogram.create scene ?widths ?colors ?depths ?name ?border (`Grid (x, z, y))
  let add_grid_histogram {scene; _} = create_grid_histogram scene

  let create_list_histogram scene ?name ?border ?widths ?depths ?colors centers =
    let widths = option_map float32_array widths in
    let depths = option_map float32_array depths in
    let colors = option_map flatten_triple_array colors in
    let centers = flatten_triple_array centers in
    Histogram.create scene ?widths ?colors ?depths ?name ?border (`List centers)
  let add_list_histogram {scene; _} = create_list_histogram scene

end

module Surface =
  struct
    module Surface = Webgl_plot_surface

    type t = Surface.t

    let set_alpha surface x = surface # set_alpha x
    let set_wireframe surface x = surface # set_wireframe x
    let set_magnetic surface x = surface # set_magnetic x
    let x_projection (surface : t) x =
     match surface # x_projection x with
       | None -> None
       | Some (z,y) -> Some (array_of_float32 z, array_of_float32 y)
    let z_projection surface z =
     match surface # z_projection z with
       | None -> None
       | Some (x,y) -> Some (array_of_float32 x, array_of_float32 y)

    let create_surface scene ?colors ?wireframe ?name ?alpha ?magnetic ~x ~z ~y () =
      let colors = option_map flatten_triple_array_array colors in
      let x = float32_array x in
      let z = float32_array z in
      let y = flatten_array_array y in
      Surface.create scene ?colors ?wireframe ?name ?alpha ?magnetic ~parametric:false x z y
    let add_surface {scene; _} = create_surface scene

    let create_parametric_surface scene ?colors ?wireframe ?name ?alpha ?magnetic ~a ~b ~p () =
      let colors = option_map flatten_triple_array_array colors in
      let a = float32_array a in
      let b = float32_array b in
      let p = flatten_triple_array_array p in
      Surface.create scene ?colors ?wireframe ?name ?alpha ?magnetic ~parametric:true a b p
    let add_parametric_surface {scene; _} = create_parametric_surface scene
  end

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

    let open Histogram in
    let open Surface in
    List.iter (function
        | Export.Histogram Grid {name; x; z; y; widths; depths; colors; border} ->
          ignore (create_grid_histogram scene ?name ?border ?widths ?depths ?colors ~x ~z ~y ())
        | Histogram List {name; centers; widths; depths; colors; border} ->
          ignore (create_list_histogram scene ?name ?border ?widths ?depths ?colors centers)
        | Surface Graph {name; x; z; y; colors; wireframe; alpha; magnetic} ->
          ignore (create_surface scene ?colors ?wireframe ?name ?alpha ?magnetic ~x ~z ~y ())
        | Surface Parametric {name; a; b; p; colors; wireframe; alpha; magnetic} ->
          ignore (create_parametric_surface scene ?colors ?wireframe ?name ?alpha ?magnetic ~a ~b ~p ())
        | _ -> (* TODO *) assert false) series;

    let automatic_bounds axis =
      match axis with Some { Export.bounds = Some _; _} -> false | _ -> true
    in

    let {Geometry.x_min; x_max; y_min; y_max; z_min; z_max} = scene # bounds in

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

let on_double_click {user_state; _} f =
  user_state.on_double_click <- f

let pointer_text_formatter {scene; _} =
  scene # pointer_text_formatter

let set_pointer_text_formatter {scene; _} f =
  scene # set_pointer_text_formatter f

let update_pre_render_hook {scene; _} f =
  scene # set_pre_render_hook (f scene # pre_render_hook)

let update_post_render_hook {scene; _} f =
  scene # set_post_render_hook (f scene # post_render_hook)

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
