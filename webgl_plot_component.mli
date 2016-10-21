module Math = Webgl_plot_math
module Helper = Webgl_plot_dom_helper

type state = {
  mutable aspect : float;
  mutable angle : float * float * float;
  mutable move : float * float * float;
  mutable dragging : bool;
  mutable pointer : float * float;
  mutable width : float;
  mutable height : float;
}

class type context =
  object
    method alt_down : bool
    method new_textbox :
      < element : Js_windows.Element.t;
        set_position : float * float -> unit;
        set_text : string -> unit >
    method set_cursor_visibility : bool -> unit
  end

val create_webgl_canvas :
  (Webgl.context ->
   context ->
   'a * (float -> state -> unit)) ->
  Js_windows.Element.t * state * 'a
