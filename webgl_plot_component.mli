(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

type state = {
  mutable aspect : float;
  mutable angle : float * float * float;
  mutable move : float * float * float;
  mutable dragging : bool;
  mutable pointer : float * float;
  mutable width : float;
  mutable height : float;
  mutable on_double_click: (unit -> unit);
}

type component = {
   root: Js_browser.Element.t;
   canvas: Js_browser.Element.t;
   overlap: Js_browser.Element.t;
   state: state;
}

class type context =
  object
    method mod_down : bool
    method new_textbox :
      < element : Js_browser.Element.t;
        set_position : float * float -> unit;
        set_text : string -> unit >
    method set_cursor_visibility : bool -> unit
  end

val create_webgl_canvas :
  (Webgl.context ->
   context ->
   'a * (float -> state -> unit)) ->
  component * 'a
