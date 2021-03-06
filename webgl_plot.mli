(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

(** A webgl plotting library. *)

type plot
(** Representation of a plot scene. *)

module Export = Webgl_plot_export
(** Alias for the export module. *)

val create : ?initial_value:Export.chart -> unit -> plot
(** Creates a new plot.

  Note: The DOM element is not attached at initialization, you
  need to manually append it to some parent. *)

val element : plot -> Js_browser.Element.t
(** Returns the DOM element carrying the plot. *)

val overlap : plot -> Js_browser.Element.t
(** Returns a div element which is exactly overlapping the canvas. *)

val selected_object : plot -> int option
(** Returns the internal id of the object currently under the cursor. *)

val pointer_projection : plot -> float * float * float
(** Current projection of the pointer (not it is only meaningful when selected_object is not None. *)

val pointer_magnetic : plot -> float * float * float
(** The selected object may attract the pointer to some position close to current projection. *)

val on_double_click: plot -> (unit -> unit) -> unit
(** Register a callback called when a double click is performed by the user. *)

val pointer_text_formatter: plot -> (Js_browser.Element.t -> unit)
(** Return the callback used to format the text below the pointer.*)

val set_pointer_text_formatter: plot -> (Js_browser.Element.t -> unit) -> unit
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

  val id : t -> int
  (** Return the internal id of a histogram. *)

  val name : t -> string
  (** Return the name of a histogram. *)

  val add_grid_histogram :
    plot ->
    ?name:string ->
    ?border:float ->
    ?widths:float array array ->
    ?depths:float array array ->
    ?floors:float array array ->
    ?colors:(float * float * float) array array ->
    x:float array ->
    z:float array -> y:float array array -> unit -> t
    (** Adds a grid histogram of [(n - 1) * (m - 1)] boxes where [n] (resp. [m])
        is the length of [x] (resp. [z]). The matrix [y] should have dimension [(n-1) * (m-1)].
        The center of the [i,j] box is :
          {C ([x]{_i} + [x]{_i+1})) / 2, [y]_{i,j}, ([z]{_i} + [z]{_i+1})) / 2 }

        - [border] is the size of the border (default: 1),
        - [widths]{_i,j} (resp. [depths]{_i,j}) should be a number between 0 and 1 that control the width (resp. the depth of the box),
        - [floors]{_i,j} is the 'ground level' of the box (i,j) (default: 0.0),
        - [colors]{_i,j} is the RGB code (between 0.0 and 1.0) of each box. *)

  val add_list_histogram :
    plot ->
    ?name:string ->
    ?border:float ->
    ?widths:float array ->
    ?depths:float array ->
    ?floors:float array ->
    ?colors:(float * float * float) array -> (float * float * float) array -> t
    (** Adds a histrogram from an array of centers [x,y,z].

     Each box is defined from the rectangle of corners:
     {C ([x] - 0.5 * [width]_k, [y], [z] - 0.5 * [depth]_k) and  ([x] + 0.5 * [width]_k, [y], [z] + 0.5 * [depth]_k) }

     - [border] is the size of the border (default: 1),
     - [floor]{_k} is the 'ground level' of each box (default : 0.0),
     - [colors]{_k} is the RGB code (between 0.0 and 1.0) of each box. *)

  val get: plot -> int -> t option
  (** Retrieve a histogram from its id. *)

  val get_from_name: plot -> string -> t list
  (** Retrieve all histograms of a given name. *)

  val set_alpha: t -> float option -> unit
  (** Set the alpha state of a histogram. *)

  val set_border: t -> float -> unit
  (** Set the border size of a histogram. *)
end


(** {6 Surfaces } **)

module Surface : sig
  type t
  (** The type of surfaces. *)

  val id : t -> int
  (** Return the internal id of a surface. *)

  val name : t -> string
  (** Return the name of a surface. *)

  val add_surface :
    plot ->
    ?colors:(float * float * float) array array ->
    ?wireframe:bool ->
    ?name:string ->
    ?alpha:float ->
    ?magnetic:bool ->
    ?crosshair: bool ->
    (float * float * float) array array -> t
    (** Adds a surface specifed by the graph of a function, [y{_i,j} = f(x{_i},z{_j})].

      - [alpha] when specified the surface will be transparent (and the value of alpha between 0 and 1 controls the opacity),
      - [magnetic] when on the mouse will be attraced by the (x,y,z) provided,
      - [wireframe] when on the wireframe will be displayed,
      - [colors] is the RGB code at each points (the color between each point will be interpolated). *)

  val get: plot -> int -> t option
  (** Retrieve a surface from its id. *)

  val get_from_name: plot -> string -> t list
  (** Retrieve all surfacess of a given name. *)

  val set_alpha: t -> float option -> unit
  (** Set the alpha state of a histogram. *)

  val set_wireframe: t -> bool -> unit
  (** Activate or deactivate the rendering of the wireframe. *)

  val set_magnetic: t -> bool -> unit
  (** Activate or deactivate the magnetism of the surface. *)

  val set_crosshair: t -> bool -> unit
  (** Activate or deactivate the display of x-z projection on the surface. *)

  val x_projection: t -> float -> (float * float) list
  (** Returns the projection along the x-axis. *)

  val y_projection: t -> float -> (float * float) list
  (** Returns the projection along the y-axis. *)

  val z_projection: t -> float -> (float * float) list
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

val remove: plot -> int -> unit
(** Remove a object (eg. a surface or a histogram) of a given id. *)

val screenshot: plot -> (string -> unit) -> unit
(** Next frame will be recorded as a data URI (in PNG) and passed to the input function.. *)

val lines_from_segments: (float * float) list -> (float * float) list list
