open Js_bindings
open Html
open Canvas.Flat

let rainbow context width =
  let gradient = create_linear_gradient context 0.0 0.0 width 0.0 in
  add_color_stop gradient 0.0 CssColor.red;
  add_color_stop gradient 0.5 CssColor.green;
  add_color_stop gradient 1.0 CssColor.blue;
  set_fill_style context (`Gradient gradient)


let real_size = 1024
let full_size = 1023.0
let dead_size = (float real_size) -. full_size
let size = full_size /. 3.0

let tick_height = 0.03
let line_width = 4.0
let font_size = 4.0 *. line_width
let font = Printf.sprintf "%.0fpx Arial" font_size

type ticks = {
  number : int;
  text: int -> string
}

let uniform_ticks number x_min x_max = {
  number;
  text = (fun k ->
     Printf.sprintf "%2f" (x_min +. (float k) *. (x_max -. x_min) /. (float number)));
}

let write_numbers context flip { number; text } =
  let w = size /. (float number) in
  let h = size *. tick_height in
  begin_path context;
  for k = 1 to number - 1 do
    let text = text (number - k) in
    translate context 0.0 (float k *. w);
    if flip then begin
      move_to context 0.0 0.0;
      line_to context (-. h) 0.0;
      let text_width = TextMetrics.width (measure_text context text) in
      fill_text context text (-. h -. line_width -. text_width) (+. 0.5 *. (font_size -. line_width));
    end else begin
      move_to context 0.0 0.0;
      line_to context h 0.0;
      fill_text context text (h +. line_width) (+. 0.5 *. (font_size -. line_width));
    end;
    translate context 0.0 (-. (float k *. w));
  done;
  close_path context;
  stroke context

let draw_ticks context face position flip ticks =
  let face_position = float (face * real_size) +. size in
  translate context face_position size;
    let corner_x, corner_y, theta, flip =
      let open Math in
      match position, flip with
      | `Top, true -> 0.0, 0.0, -. (0.5 *. pi), false
      | `Top, false -> size, 0.0, 0.5 *. pi, true
      | `Left, false -> 0.0, 0.0, 0.0, true
      | `Left, true -> 0.0, size, -. pi, false
      | `Right, true -> size, size, -. pi, true
      | `Right, false -> size, 0.0, 0.0, false
      | `Bottom, true -> 0.0, size, -. (0.5 *. pi), true
      | `Bottom, false -> size, size, 0.5 *. pi, false
    in
    translate context corner_x corner_y;
      rotate context theta;
        write_numbers context flip ticks;
      rotate context (-. theta);
    translate context (-. corner_x) (-. corner_y);
  translate context (-. face_position) (-. size)

let create_grid_texture document x_ticks y_ticks z_ticks =
  let canvas = Document.create_html_canvas document in
  Canvas.set_width canvas (8*real_size);
  Canvas.set_height canvas real_size;

  let context =
    match get_context canvas with
    | None -> failwith "get_context"
    | Some x -> x
  in
  clear_rect context 0.0 0.0 (float real_size) (float real_size);
  set_line_width context line_width;
  set_font context font;
  for k = 0 to 5 do
    let face_position = float (k * real_size) +. size in
    stroke_rect context face_position size size size;
    fill_text context (Printf.sprintf "Face %d" k) face_position (0.4 *. size);
  done;
  draw_ticks context 0 `Left false y_ticks;
  draw_ticks context 0 `Top false x_ticks;
  draw_ticks context 0 `Bottom false x_ticks;
  draw_ticks context 0 `Right false y_ticks;

  draw_ticks context 2 `Left false z_ticks;
  draw_ticks context 2 `Top false x_ticks;
  draw_ticks context 2 `Bottom false x_ticks;
  draw_ticks context 2 `Right false z_ticks;

  canvas

