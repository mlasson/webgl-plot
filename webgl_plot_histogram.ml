open Js_array
open Webgl_plot_misc
open Webgl_plot_math
open Webgl_plot_drawable

module Geometry = Webgl_plot_geometry
module Shaders = Webgl_plot_shaders
module Intersection = Webgl_plot_intersection

let create gl (shader : Shaders.Basic.shader) ?(name = "") ?widths ?depths ?colors ?(border = 0.001) input =
  let open Shaders in
  let open Geometry in
  let {Histogram.triangles; normals; shrink_directions; colors} =
    Histogram.create ?widths ?depths ?colors input
  in
  let a_triangles = create_attrib_array gl 3 triangles in
  let a_normals = create_attrib_array gl 3 normals in
  let a_shrink_directions = create_attrib_array gl 3 shrink_directions in
  let a_colors = create_attrib_array gl 3 colors in
  let a_border_colors = create_attrib_array gl 3
    (FloatData.init3 (Float32Array.length triangles) (fun _ -> 0.0, 0.0, 0.0))
  in
  object
    inherit dummy_ray
    val alpha = 0.7
    val mutable scale = (1., 1., 1.)
    val mutable position = (0., 0., 0.)
    val mutable border = border
    val name = name

    method opaque = true

    method set_scale x =
      scale <- x

    method set_position x =
      position <- x

    method draw (ctx : context) shader_id round =
      if shader_id = shader # id && round = 0 then begin

        let x_border, y_border, z_border =
          let x_scale, y_scale, z_scale = ctx # scale in
          -. border *. x_scale,
          -. border *. y_scale,
          -. border *. z_scale
        in
        shader # set_alpha alpha;
        shader # set_object_matrix
          (float32_array (Vector.to_array
             (Vector.Const.scale_translation
                (Vector.of_three scale) (Vector.of_three position))));
        shader # set_normals a_normals;
        shader # set_shrink_directions a_shrink_directions;
        shader # set_positions a_triangles;
        shader # set_colors a_border_colors;
        shader # set_shrink (0.0, 0.0, 0.0);
        shader # set_explode 0.0;
        shader # draw_arrays Shaders.Triangles (a_triangles # count);
        shader # set_shrink (x_border, y_border, z_border);
        shader # set_explode 0.0001;
        shader # set_colors a_colors;
        shader # draw_arrays Shaders.Triangles (a_triangles # count);
      end
  end


