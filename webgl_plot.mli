(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

(** Webgl-plot **)

type plot
(** Representation of a plot scene. *)

module Export : module type of Webgl_plot_export
(** Alias for the export module. *)


val create : ?initial_value:Export.chart -> unit -> plot
(** Creates a new plot.
 *
 * Note: The DOM element is not attached at initialization, you
 * need to manually append it to some parent.
 * *)

val element : plot -> Js_windows.Element.t
(** Retunrs the DOM element carrying the plot. *)

val selected_object : plot -> string option
(** Returns the object currently under the cursor. *)

val pointer_projection : plot -> float * float * float
(** Current projection of the pointer (not it is only meaningful when selected_object is not None. *)

val pointer_magnetic : plot -> float * float * float
(** The selected object may attract the pointer to some position close to current projection. *)


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
