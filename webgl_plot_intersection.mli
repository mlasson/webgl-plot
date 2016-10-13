open Js_array
open Webgl_plot_misc
module Math = Webgl_plot_math

type rect = {
  x_min : float;
  z_min : float;
  x_max : float;
  z_max : float;
}
type ray_table = (rect * (int * int * int) list) list
val build_ray_table :
  Float32Array.t ->
  Index.t -> ray_table
val ray_triangles :
  Float32Array.t ->
  (rect * (int * int * int) list) list ->
  Math.three Math.Vector.vector ->
  Math.three Math.Vector.vector -> Math.three Math.Vector.vector option
