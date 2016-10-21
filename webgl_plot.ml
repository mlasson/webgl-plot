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

let create {Export.x_axis; y_axis; z_axis; series; ratio} =
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

    scene, fun clock {Component.aspect; angle; move; pointer; width; height; _} ->
      scene # set_clock clock;

      scene # set_aspect aspect;
      scene # set_angle angle;
      scene # set_move move;
      scene # set_pointer pointer;

      scene # set_width (int_of_float width);
      scene # set_height (int_of_float height);
      scene # render
  in
  let element, _state, _scene = Component.create_webgl_canvas renderer in
  element

let () =
  let o = Ojs.empty_obj () in
  Ojs.set Ojs.global "WebglPlot" o;
  Ojs.set o "create" ([%js.of: Export.chart -> Element.t] create)
