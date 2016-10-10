open Js_bindings
open Canvas

type ticks = {
  number : int;
  text: int -> string
}

let format_from_range r =
  if r < 1e-4 || r > 1e7 then
    Printf.sprintf "%.3g"
  else if r < 10.0 then
    let d = - int_of_float (log10 r) + 2 in
    Printf.sprintf "%.*f" d
  else if r < 1000.0 then
    Printf.sprintf "%.2f"
  else
    Printf.sprintf "%6.0f"

let uniform_ticks number x_min x_max = {
  number;
  text = (fun k ->
     format_from_range (x_max -. x_min) (x_min +. (float k) *. (x_max -. x_min) /. (float number)));
}

(*
            2 × real_size
        ----------------------
              |       |         | 0.5 * (real_size - inner_size)
            A + A   E + E       ]
            B + B   D + D       ]
            C + C   C + C       ] inner_size
            D + D   B + B       ]
            E + E   A + A       ]
              |       |         | 0.5 * (real_size - inner_size)
        ----------------------
*)

let stack : [`Translate of float * float | `Rotate of float] Stack.t = Stack.create ()

let push context t =
 Stack.push t stack;
 match t with
 | `Translate (x,y) -> translate context x y
 | `Rotate rho -> rotate context rho

let pop context =
 match Stack.pop stack with
 | `Translate (x,y) -> translate context (-. x) (-. y)
 | `Rotate rho -> rotate context (-. rho)

let tick_height = 0.03
let real_size = 512
let inner_size = 0.9 *. (float real_size)
let line_width = float real_size /. 256.0
let font_size = 10.0 *. line_width
let font = Printf.sprintf "%.0fpx Arial" font_size

let create_face_texture () =
  let canvas = Document.create_element document "canvas" in
  Element.set_attribute canvas "width" (string_of_int real_size);
  Element.set_attribute canvas "height" (string_of_int real_size);
  let context =
    match get_context canvas with
    | None -> failwith "get_context"
    | Some x -> x
  in
  let size = float real_size in
  clear_rect context 0.0 0.0 size size;
  set_stroke_style context (`Color "black");
  set_line_width context (4.0 *. line_width);
  stroke_rect context 0.0 0.0 size size;
  canvas

let draw_tick context text =
  let tick_size = 0.01 *. (float real_size) in
  move_to context (-. tick_size) 0.0;
  line_to context tick_size 0.0;
  let text_width = TextMetrics.width (measure_text context text) in
  fill_text context text (-. tick_size -. line_width -. text_width) (0.5 *. (font_size -. line_width));
  fill_text context text (tick_size +. line_width) (0.5 *. (font_size -. line_width))

let create_ticks_texture label {number; text} =
  let canvas = Document.create_element document "canvas" in
  Element.set_attribute canvas "width" (string_of_int real_size);
  Element.set_attribute canvas "height" (string_of_int real_size);
  let context =
    match get_context canvas with
    | None -> failwith "get_context"
    | Some x -> x
  in
  let size = float real_size in
  clear_rect context 0.0 0.0 size size;
  set_fill_style context (`Color "black");
  set_line_width context line_width;
  set_font context font;
  let padding = 0.5 *. (size -. inner_size) in
  let space = inner_size /. (float (number)) in
  let draw_ticks text =
    begin_path context;
    for k = 1 to number-1 do
      push context (`Translate (0.0, space));
      draw_tick context (text k);
    done;
    close_path context;
    stroke context;
    for _ = 1 to number-1 do
      pop context;
    done;
    in
  let text_width = TextMetrics.width (measure_text context label) in
  push context (`Translate (0.25 *. size, padding));
  draw_ticks text;
  begin
    push context (`Translate (-. 0.25 *. size +. 0.5 *. font_size, 0.5 *. (size -. text_width)));
    push context (`Rotate (0.5 *. Math.pi));
      fill_text context label 0.0 0.0;
    pop context;
    pop context;

    push context (`Translate (0.25 *. size -. 0.5 *. font_size, 0.5 *. (size +. text_width)));
    push context (`Rotate (-0.5 *. Math.pi));
      fill_text context label 0.0 0.0;
    pop context;
    pop context;
  end;


  pop context;

  push context (`Translate (0.75 *. size, padding));
  draw_ticks (fun k-> text (number - k));
  begin
    push context (`Translate (-. 0.25 *. size +. 0.5 *. font_size, 0.5 *. (size -. text_width)));
    push context (`Rotate (0.5 *. Math.pi));
    fill_text context label 0.0 0.0;
    pop context;
    pop context;
    push context (`Translate (0.25 *. size -. 0.5 *. font_size, 0.5 *. (size +. text_width)));
    push context (`Rotate (-0.5 *. Math.pi));
    fill_text context label 0.0 0.0;
    pop context;
    pop context;
  end;

  pop context;
  assert (Stack.is_empty stack);
  if false then
    Element.append_child (Document.body document) canvas;
  canvas
