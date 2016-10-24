(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

(** A webgl plotting library. *)

type plot
(** Representation of a plot scene. *)

module Export : module type of Webgl_plot_export
(** Alias for the export module. *)


val create : ?initial_value:Export.chart -> unit -> plot
(** Creates a new plot.

  Note: The DOM element is not attached at initialization, you
  need to manually append it to some parent. *)

val element : plot -> Js_windows.Element.t
(** Returns the DOM element carrying the plot. *)

val selected_object : plot -> string option
(** Returns the object currently under the cursor. *)

val pointer_projection : plot -> float * float * float
(** Current projection of the pointer (not it is only meaningful when selected_object is not None. *)

val pointer_magnetic : plot -> float * float * float
(** The selected object may attract the pointer to some position close to current projection. *)

val on_double_click: plot -> (unit -> unit) -> unit
(** Register a callback called when a double click is performed by the user. *)

val pointer_text_formatter: plot -> (Js_windows.Element.t -> unit)
(** Return the callback used to format the text below the pointer.*)

val set_pointer_text_formatter: plot -> (Js_windows.Element.t -> unit) -> unit
(** Set the callback used to format the text below the pointer.*)

val update_pre_render_hook: plot -> ((unit -> unit) -> unit -> unit) -> unit
(** Update the callback called before rendering each frame (the argument's argument is the previous callback).

    The default pre render hook does nothing. *)

val update_post_render_hook: plot -> ((unit -> unit) -> unit -> unit) -> unit
(** Update the callback called after rendering each frame (the argument's argument is the previous callback).

    The default pre render hook does nothing. *)


(** {6 Histograms } **)


module Histogram : sig

  type t
  (** The type of histograms. *)

  val add_grid_histogram :
    plot ->
    ?name:string ->
    ?border:float ->
    ?widths:float array array ->
    ?depths:float array array ->
    ?colors:(float * float * float) array array ->
    x:float array ->
    z:float array -> y:float array array -> unit -> t
    (** Adds a grid histogram of [(n - 1) * (m - 1)] boxes where [n] (resp. [m])
        is the length of [x] (resp. [z]). The matrix [y] should have dimension [(n-1) * (m-1)].
        The center of the [i,j] box is :
          {C ([x]{_i} + [x]{_i+1})) / 2, [y]_{i,j}, ([z]{_i} + [z]{_i+1})) / 2 }

        - [border] is the size of the border (default: 1),
        - [widths]{_i,j} (resp. [depths]{_i,j}) should be a number between 0 and 1 that control the width (resp. the depth of the box),
        - [colors]{_i,j} id the RGB code (between 0.0 and 1.0) of each box. *)

  val add_list_histogram :
    plot ->
    ?name:string ->
    ?border:float ->
    ?widths:float array ->
    ?depths:float array ->
    ?colors:(float * float * float) array -> (float * float * float) array -> t
    (** Adds an histrogram from an array of centers [x,y,z].

     Each box is defined from the rectangle of corners:
     {C ([x] - 0.5 * [width]_k, [y], [z] - 0.5 * [depth]_k) and  ([x] + 0.5 * [width]_k, [y], [z] + 0.5 * [depth]_k) }

     - [border] is the size of the border (default: 1),
     - [colors]{_k} id the RGB code (between 0.0 and 1.0) of each box. *)


  val set_alpha: t -> float option -> unit
  (** Set the alpha state of an histogram. *)

  val set_border: t -> float -> unit
  (** Set the border size of an histogram. *)
end


(** {6 Surfaces } **)

module Surface : sig
  type t
  (** The type of surfaces. *)

  val add_surface :
    plot ->
    ?colors:(float * float * float) array array ->
    ?wireframe:bool ->
    ?name:string ->
    ?alpha:float ->
    ?magnetic:bool ->
    x:float array ->
    z:float array -> y:float array array -> unit -> t
    (** Adds a surface specifed by the graph of a function, [y{_i,j} = f(x{_i},z{_j})].

      - [alpha] when specified the surface will be transparent (and the value of alpha between 0 and 1 controls the opacity),
      - [magnetic] when on the mouse will be attraced by the (x,y,z) provided,
      - [wireframe] when on the wireframe will be displayed,
      - [colors] is the RGB code at each points (the color between each point will be interpolated). *)


  val add_parametric_surface :
    plot ->
    ?colors:(float * float * float) array array ->
    ?wireframe:bool ->
    ?name:string ->
    ?alpha:float ->
    ?magnetic:bool ->
    a:float array ->
    b:float array -> p:(float * float * float) array array -> unit -> t
    (** Adds a surface specifed by a parametric surfaceo f, [(x,y,z) = (x{_i},f(x{_i},z{_j}), z{_j})].

        - [alpha] when specified the surface will be transparent (and the value of alpha between 0 and 1 controls the opacity),
        - [magnetic] when on the mouse will be attraced by the (x,y,z) provided,
        - [wireframe] when on the wireframe will be displayed,
        - [colors] is the RGB code at each points (the color between each point will be interpolated). *)


  val set_alpha: t -> float option -> unit
  (** Set the alpha state of an histogram. *)

  val set_wireframe: t -> bool -> unit
  (** Activates or deactivate the rendering of the wireframe. *)

  val set_magnetic: t -> bool -> unit
  (** Activates or deactivate the magnetism of the surface. *)

  val x_projection: t -> float -> (float array * float array) option
  (** Returns the projection along the x-axis. *)

  val z_projection: t -> float -> (float array * float array) option
  (** Returns the projection along the z-axis. *)

end


(** {6 Point of view} **)


val angle : plot -> float * float * float
(** Returns [angle_x, angle_y, angle_z] three angles in radians representing the current rotation around the center of the plot. *)

val move : plot -> float * float * float
(** Returns the position of the center of the plot [move_x, move_y, move_z] in screen coordinates (each number is between -1 and 1). *)

val set_angle : plot -> float * float * float -> unit
(** Sets the angle. *)

val set_move : plot -> float * float * float -> unit
(** Sets the position of the center of the plot in screen coordinates. *)


(** {6 Control of axes.} **)


val set_x_axis_label : plot -> string -> unit
(** Sets the label on the X axis. *)

val set_y_axis_label : plot -> string -> unit
(** Sets the label on the Y axis. *)

val set_z_axis_label : plot -> string -> unit
(** Sets the label on the Z axis. *)

val set_x_axis_bounds : plot -> float * float -> unit
(** Sets the boundaris of the X axis. *)

val set_y_axis_bounds : plot -> float * float -> unit
(** Sets the boundaris of the Y axis. *)

val set_z_axis_bounds : plot -> float * float -> unit
(** Sets the boundaris of the Z axis. *)

val set_x_axis_ticks : plot -> Export.tick list -> unit
(** Sets the ticks text and positions on the X axis. *)

val set_y_axis_ticks : plot -> Export.tick list -> unit
(** Sets the ticks text and positions on the Y axis. *)

val set_z_axis_ticks : plot -> Export.tick list -> unit
(** Sets the ticks text and positions on the Z axis. *)
