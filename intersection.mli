open Webgl

type ray_table

val build_ray_table :
  < pop : unit; progress : float -> unit; push : unit; status : string -> unit;
    .. > ->
  Float32Array.t ->
  Geometry.Index.t ->
  ray_table Asynchronous_computations.t

val ray_triangles :
  Float32Array.t ->
  ray_table ->
  Math.three Math.Vector.vector ->
  Math.three Math.Vector.vector -> Math.three Math.Vector.vector option
