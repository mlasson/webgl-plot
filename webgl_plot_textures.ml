(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

open Js_browser
open Canvas

module Math = Webgl_plot_math

type ticks = {
  values: float array;
  texts: string array;
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

let stack : [`Translate of float * float | `Scale of float * float | `Rotate of float] Stack.t = Stack.create ()

let push context t =
 Stack.push t stack;
 match t with
 | `Translate (x,y) -> translate context x y
 | `Scale (x,y) -> scale context x y
 | `Rotate rho -> rotate context rho

let pop context =
 match Stack.pop stack with
 | `Translate (x,y) -> translate context (-. x) (-. y)
 | `Scale (x,y) -> scale context (1. /. x) (1. /. y)
 | `Rotate rho -> rotate context (-. rho)

let tick_height = 0.03
let real_size = 1024
let inner_size = 0.9 *. (float real_size)
let line_width = float real_size /. 256.0
let font_size = 10.0 *. line_width

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
  let new_text = ref text in
  let new_font_size = ref font_size in
  let max_width = 0.25 *. inner_size in
  let text_width = ref 0.0 in
  while
    set_font context (Printf.sprintf "%.0fpx Arial" !new_font_size);
    text_width := TextMetrics.width (measure_text context !new_text);
    !text_width > max_width && String.length !new_text > 4 do
    if false then
      Printf.printf "new = %S, width = %g < %g, size = %g\n%!" !new_text !text_width max_width !new_font_size;
    if !new_font_size > 1.1 *. font_size then
      new_font_size := 0.9 *. !new_font_size
    else
      new_text := (String.sub !new_text 0 ((String.length !new_text) - 4))^"..."
  done;
  fill_text context !new_text (-. tick_size -. line_width -. !text_width) (0.5 *. (font_size -. line_width));
  fill_text context !new_text (tick_size +. line_width) (0.5 *. (font_size -. line_width));
  set_font context (Printf.sprintf "%.0fpx Arial" font_size)

let create_ticks_texture ratio label {values; texts} =
  let canvas = Document.create_element document "canvas" in
  let number = Array.length values in
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
  set_font context (Printf.sprintf "%.0fpx Arial" font_size);
  let padding = 0.5 *. (size -. inner_size) in
  let size_y = inner_size *. ratio in
  let draw_ticks reverse =
    begin_path context;
    for k = 0 to number-1 do
      let y, s =
        if reverse then
          let k = number - 1 - k in
          (1.0 -. values.(k)) *. size_y, texts.(k)
        else
          values.(k) *. size_y, texts.(k)
      in
      push context (`Translate (0.0, y));
      draw_tick context s;
      pop context;
    done;
    close_path context;
    stroke context;
  in
  let text_width = TextMetrics.width (measure_text context label) in
  push context (`Scale (1.0, 1.0 /. ratio));
  push context (`Translate (0.25 *. size, padding *. ratio));
  draw_ticks false;
  begin
    push context (`Translate (-. 0.25 *. size +. 0.5 *. font_size, 0.5 *. (size -. text_width) *. ratio));
    push context (`Rotate (0.5 *. Math.pi));
      fill_text context label 0.0 0.0;
    pop context;
    pop context;

    push context (`Translate (0.25 *. size -. 0.5 *. font_size, 0.5 *. (size +. text_width) *. ratio));
    push context (`Rotate (-0.5 *. Math.pi));
      fill_text context label 0.0 0.0;
    pop context;
    pop context;
  end;


  pop context;

  push context (`Translate (0.75 *. size, padding *. ratio));
  draw_ticks true;
  begin
    push context (`Translate (-. 0.25 *. size +. 0.5 *. font_size, 0.5 *. (size -. text_width) *. ratio));
    push context (`Rotate (0.5 *. Math.pi));
    fill_text context label 0.0 0.0;
    pop context;
    pop context;
    push context (`Translate (0.25 *. size -. 0.5 *. font_size, 0.5 *. (size +. text_width) *.ratio));
    push context (`Rotate (-0.5 *. Math.pi));
    fill_text context label 0.0 0.0;
    pop context;
    pop context;
  end;

  pop context;
  pop context;
  assert (Stack.is_empty stack);
  if false then
    Element.append_child (Document.body document) canvas;
  canvas
