open Js_bindings
open Html
open Canvas.Svg

let rainbow context width =
  let gradient = create_linear_gradient context 0.0 0.0 width 0.0 in
  add_color_stop gradient 0.0 CssColor.red;
  add_color_stop gradient 0.5 CssColor.green;
  add_color_stop gradient 1.0 CssColor.blue;
  set_fill_style context (`Gradient gradient)

let ticks = 10
let tick_height = 0.03

let draw_ticks context width height =
  let w = width /. (float ticks) in
  let h = tick_height *. height in
  begin_path context;
  for k = 1 to ticks - 1 do
    move_to context (w *. (float k)) 0.0;
    line_to context (w *. (float k)) h;
    move_to context 0.0 (w *. (float k));
    line_to context h (w *. (float k));
    move_to context (w *. (float k)) height;
    line_to context (w *. (float k)) (height -. h);
    move_to context width (w *. (float k));
    line_to context (width -. h) (w *. (float k))
  done;
  close_path context;
  stroke context

let create_grid_texture ?(width = 512) ?(height = 512) document =
  let canvas = Document.create_html_canvas document in
  Canvas.set_width canvas width;
  Canvas.set_height canvas height;

  let context =
    match get_context canvas with
    | None -> failwith "get_context"
    | Some x -> x
  in
  let width = float width in
  let height = float height in
  set_line_width context 2.0;
  stroke_rect context 0.0 0.0 width height;
  draw_ticks context width height;
  canvas

