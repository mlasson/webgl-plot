open Js_windows
open Js_array

module Geometry = Webgl_plot_geometry
module Scene = Webgl_plot_scene
module Component = Webgl_plot_component
module Math = Webgl_plot_math
module Helper = Webgl_plot_dom_helper
module Export = Webgl_plot_export

let default_option x = function
  | None -> x
  | Some y -> y

let option_iter o f =
  match o with
  | None -> ()
  | Some x -> f x

let option_map f o =
  match o with
  | None -> None
  | Some x -> Some (f x)

let float32_array a =
  Float32Array.new_float32_array (`Data a)

let float32_array_array aa =
  let size =
    let res = ref 0 in
    for k = 0 to Array.length aa - 1 do
      res := !res + Array.length aa.(k);
    done;
    !res
  in
  let res = Float32Array.new_float32_array (`Size size) in
  let pos = ref 0 in
  for i = 0 to Array.length aa - 1 do
    let a = aa.(i) in
    for j = 0 to Array.length a - 1 do
      Float32Array.set res !pos a.(j);
      incr pos;
    done;
  done;
  res

let float32_array_array_array aaa =
  let size =
    let res = ref 0 in
    for i = 0 to Array.length aaa - 1 do
      let aa = aaa.(i) in
      for j = 0 to Array.length aa - 1 do
        res := !res + Array.length aa.(j);
      done;
    done;
    !res
  in
  let res = Float32Array.new_float32_array (`Size size) in
  let pos = ref 0 in
  for i = 0 to Array.length aaa - 1 do
    let aa = aaa.(i) in
    for j = 0 to Array.length aa - 1 do
      let a = aa.(j) in
      for k = 0 to Array.length a - 1 do
        Float32Array.set res !pos a.(k);
        incr pos;
      done;
    done;
  done;
  res

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

    List.iter (function
        | Histogram Uniform {name; x; z; y; widths; colors; wireframe;} ->
          let widths = option_map float32_array_array widths in
          let colors = option_map float32_array_array_array colors in
          let x = float32_array x in
          let z = float32_array z in
          let y = float32_array_array y in
          scene # add_uniform_histogram ?widths ?colors ?wireframe ?name x z y

        | Histogram Parametric {name; a; b; p; widths; colors; wireframe;} ->
          let widths = option_map float32_array_array widths in
          let colors = option_map float32_array_array_array colors in
          let a = float32_array a in
          let b = float32_array b in
          let p = float32_array_array_array p in
          scene # add_parametric_histogram ?widths ?colors ?wireframe ?name a b p

        | Scatter Uniform {name; x; z; y; widths; colors; wireframe;} ->
          let widths = option_map float32_array_array widths in
          let colors = option_map float32_array_array_array colors in
          let x = float32_array x in
          let z = float32_array z in
          let y = float32_array_array y in
          scene # add_uniform_scatter ?widths ?colors ?wireframe ?name x z y

        | Scatter Parametric {name; a; b; p; widths; colors; wireframe;} ->
          let widths = option_map float32_array_array widths in
          let colors = option_map float32_array_array_array colors in
          let a = float32_array a in
          let b = float32_array b in
          let p = float32_array_array_array p in
          scene # add_parametric_scatter ?widths ?colors ?wireframe ?name a b p

        | Surface Uniform {name; x; z; y; colors; wireframe;} ->
          let colors = option_map float32_array_array_array colors in
          let x = float32_array x in
          let z = float32_array z in
          let y = float32_array_array y in
          scene # add_uniform_surface ?colors ?wireframe ?name x z y

        | Surface Parametric {name; a; b; p; colors; wireframe;} ->
          let colors = option_map float32_array_array_array colors in
          let a = float32_array a in
          let b = float32_array b in
          let p = float32_array_array_array p in
          scene # add_parametric_surface ?colors ?wireframe ?name a b p
        | _ -> (* TODO *) assert false) series;

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


(*
let () = Window.set_onload window (fun _ ->
    let open Export in
    let series = [
      Histogram (Uniform {
          name = Some "test";
          x = [| 1.0 |];
          z = [| 1.0 |];
          y = [| [| 1.0 |] |];
          widths = Some [| [| 1.0 |] |];
          colors = Some [| [| [| 1.0; 1.0; 1.0; 1.0 |]|]|];
          wireframe = Some true;
        })
    ] in
    let text = JSON.stringify (chart_to_js {
        x_axis = None;
        y_axis = None;
        z_axis = None;
        series;
        pointer_kind = None;
        magnetic = None;
        ratio = None;
      })
    in
    Helper.create ~text ~parent:(Document.body document) "code" [] |> ignore) *)
