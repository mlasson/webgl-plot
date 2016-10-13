open Js_array
open Webgl_plot_misc
open Webgl_plot_math
open Webgl_plot_drawable

module Geometry = Webgl_plot_geometry
module Shaders = Webgl_plot_shaders
module Intersection = Webgl_plot_intersection

let create gl (shader : Shaders.Basic.shader) xs zs ys =
  let open Shaders in
  let open Geometry in
  let min, max = match FloatData.min_max ys with Some c -> c | None -> 0.0, 1.0 in
  let range = max -. min in
  let rainbow y =
      Color.white_cold_to_hot ((y -. min) /. range)
  in
  let xs = array_of_float32 xs in
  let ys = array_of_float32 ys in
  let zs = array_of_float32 zs in
  let {Histogram.triangles; normals; wireframe; normals_wireframe} = Histogram.create xs zs ys in
  let a_triangles = create_attrib_array gl 3 triangles in
  let a_normals = create_attrib_array gl 3 normals in
  let a_colors = create_attrib_array gl 3 (* rainbow *)
    (FloatData.init3 (Float32Array.length triangles) (fun k ->
        rainbow (Float32Array.get triangles (3 * k + 1))))
  in
  let a_wireframe = create_attrib_array gl 3 wireframe in
  let a_normals_wireframe = create_attrib_array gl 3 normals_wireframe in
  let a_colors_wireframe =
    create_attrib_array gl 3 (* black *)
      (FloatData.init3 (Float32Array.length triangles) (fun _ -> 0.0, 0.0, 0.0))
  in
  object
    inherit dummy_ray
    val mutable scale = (1., 1., 1.)
    val mutable position = (0., 0., 0.)

    method set_scale x =
      scale <- x

    method set_position x =
      position <- x

    method draw (_ : context) shader_id =
      if shader_id = shader # id then begin
        shader # set_object_matrix
          (float32_array (Vector.to_array
             (Vector.Const.scale_translation
                (Vector.of_three scale) (Vector.of_three position))));
        shader # set_colors a_colors;
        shader # set_normals a_normals;
        shader # set_positions a_triangles;
        shader # draw_arrays Shaders.Triangles (a_triangles # count);

        shader # set_positions a_wireframe;
        shader # set_colors a_colors_wireframe;
        shader # set_normals a_normals_wireframe;
        shader # draw_arrays Shaders.Lines (a_wireframe # count)
      end
  end


