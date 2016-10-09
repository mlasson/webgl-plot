type rect = {
  x_min : float;
  z_min : float;
  x_max : float;
  z_max : float;
}
type ray_table = (rect * (int * int * int) list) list
val build_ray_table :
  Webgl.Float32Array.t ->
  Geometry.Index.t -> ray_table
val ray_triangles :
  Webgl.Float32Array.t ->
  (rect * (int * int * int) list) list ->
  Math.three Math.Vector.vector ->
  Math.three Math.Vector.vector -> Math.three Math.Vector.vector option
