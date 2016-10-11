module Math = Webgl_plot_math
module Geometry = Webgl_plot_geometry

open Js_array

type rect = {
  x_min : float;
  z_min : float;
  x_max : float;
  z_max : float;
}
type ray_table = (rect * (int * int * int) list) list
val build_ray_table :
  Float32Array.t ->
  Geometry.Index.t -> ray_table
val ray_triangles :
  Float32Array.t ->
  (rect * (int * int * int) list) list ->
  Math.three Math.Vector.vector ->
  Math.three Math.Vector.vector -> Math.three Math.Vector.vector option
