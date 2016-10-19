open Js_windows
open Webgl_plot_misc
open Js_array
open FloatData

module Geometry = Webgl_plot_geometry
module Scene = Webgl_plot_scene
module Component = Webgl_plot_component
module Math = Webgl_plot_math
module Helper = Webgl_plot_dom_helper
module Export = Webgl_plot_export

let create {Export.x_axis; y_axis; z_axis; series; pointer_kind; magnetic; ratio} =
  let open Export in
  let main = Helper.element_of_id "main" in
  let renderer gl textbox_factory =
    let open Scene in
    let scene = prepare_scene gl textbox_factory in
    let repere = scene # repere in

    option_iter magnetic (scene # set_magnetic);

    option_iter ratio (repere # set_ratio);
    option_iter pointer_kind (function
        | Cross -> scene # set_pointer_kind Cross
        | Sphere -> scene # set_pointer_kind Sphere
        | None -> scene # set_pointer_kind None
        | Unknown _ -> assert false (* TODO *)
      );

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
        | Histogram Uniform {name; x; z; y; widths; depths; colors; border} ->
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


          scene # add_uniform_histogram ?widths ?colors ?depths ?name ?border x z y

        | Histogram List {name; centers; widths; depths; colors; border} ->
          let widths = option_map float32_array widths in
          let depths = option_map float32_array depths in
          let colors = option_map flatten_array_array colors in
          let centers = flatten_array_array centers in

          update_min_max x_min x_max centers 3 0;
          y_min := min !y_min 0.0;
          update_min_max y_min y_max centers 3 1;
          update_min_max z_min z_max centers 3 2;

          scene # add_list_histogram ?widths ?colors ?depths ?name ?border centers

        | Scatter Uniform {name; x; z; y; radius; colors; } ->
          let radius = option_map flatten_array_array radius in
          let colors = option_map flatten_array_array_array colors in
          let x = float32_array x in
          let z = float32_array z in
          let y = flatten_array_array y in

          update_min_max x_min x_max x 1 0;
          update_min_max z_min z_max z 1 0;
          update_min_max y_min y_max y 1 0;
          scene # add_uniform_scatter ?radius ?colors ?name x z y

        | Scatter Parametric {name; a; b; p; radius; colors; } ->
          let radius = option_map flatten_array_array radius in
          let colors = option_map flatten_array_array_array colors in
          let a = float32_array a in
          let b = float32_array b in
          let p = flatten_array_array_array p in

          scene # add_parametric_scatter ?radius ?colors ?name a b p

        | Surface Uniform {name; x; z; y; colors; wireframe; alpha} ->
          let colors = option_map flatten_array_array_array colors in
          let x = float32_array x in
          let z = float32_array z in
          let y = flatten_array_array y in

          update_min_max x_min x_max x 1 0;
          update_min_max z_min z_max z 1 0;
          update_min_max y_min y_max y 1 0;

          scene # add_uniform_surface ?colors ?wireframe ?name ?alpha x z y

        | Surface Parametric {name; a; b; p; colors; wireframe; alpha} ->
          let colors = option_map flatten_array_array_array colors in
          let a = float32_array a in
          let b = float32_array b in
          let p = flatten_array_array_array p in

          update_min_max x_min x_max p 3 0;
          update_min_max y_min y_max p 3 1;
          update_min_max z_min z_max p 3 2;

          scene # add_parametric_surface ?colors ?wireframe ?name ?alpha a b p
        | _ -> (* TODO *) assert false) series;

    let automatic_bounds axis =
      match axis with Some { bounds = Some _; _} -> false | _ -> true
    in

    if automatic_bounds x_axis then
      repere # set_x_axis_bounds (!x_min, !x_max);

    if automatic_bounds y_axis then
      repere # set_y_axis_bounds (!y_min, !y_max);

    if automatic_bounds z_axis then
      repere # set_z_axis_bounds (!z_min, !z_max);

    let automatic_ticks axis =
      match axis with Some { ticks = Some _; _} -> false | _ -> true
    in
    let uniform_ticks ?(skip_first = false) n min max =
      let n = if n <= 3 then 3 else n in
      let d = if skip_first then 1 else 0 in
      let format = format_from_range (max -. min) in
      Array.init (n - d) (fun k ->
          let value = min +. (float (k + d)) *. (max -. min) /. (float (n - 1)) in
          { value; label = format value })
      |> Array.to_list
    in

    if ratio = None then
      repere # set_ratio (1.0, (!y_max -. !y_min) /. (!x_max -. !x_min), (!z_max -. !z_min) /. (!x_max -. !x_min));

    let number_of_ticks ratio =
      int_of_float (15.0 *. ratio)
    in
    let x_ratio, y_ratio, z_ratio = repere # ratio in
    if automatic_ticks x_axis then
      repere # set_x_axis_ticks (uniform_ticks (number_of_ticks x_ratio) !x_min !x_max);
    if automatic_ticks y_axis then
      repere # set_y_axis_ticks (uniform_ticks ~skip_first:true (number_of_ticks y_ratio) !y_min !y_max);
    if automatic_ticks z_axis then
      repere # set_z_axis_ticks (uniform_ticks (number_of_ticks z_ratio) !z_min !z_max);



    fun clock {Component.aspect; angle; move; pointer; width; height; _} ->
      scene # set_clock clock;

      scene # set_aspect aspect;
      scene # set_angle angle;
      scene # set_move move;
      scene # set_pointer pointer;

      scene # set_width (int_of_float width);
      scene # set_height (int_of_float height);
      scene # render
  in
  let component = Component.create_webgl_canvas renderer in
  component

let () =
  let o = Ojs.empty_obj () in
  Ojs.set Ojs.global "WebglPlot" o;
  Ojs.set o "create" ([%js.of: Export.chart -> Element.t] create)
