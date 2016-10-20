open Js_array
open Webgl_plot_misc
open Webgl_plot_math
open Webgl_plot_drawable

module Math = Webgl_plot_math
module Geometry = Webgl_plot_geometry
module Shaders = Webgl_plot_shaders
module Intersection = Webgl_plot_intersection

let create (scene : Webgl_plot_scene.scene) ?(name = "") ?widths ?depths ?colors ?(border = 0.001) input =
  let open Shaders in
  let open Geometry in
  let gl = scene # gl in
  let shader = scene # basic_shader in
  let {Histogram.triangles; normals; shrink_directions; colors} =
    Histogram.create ?widths ?depths ?colors input
  in
  let indexes = Index.of_array (Array.init ((Float32Array.length triangles) / 3) (fun x -> x)) in
  let table = if true then Intersection.build_ray_table triangles indexes else [] in
  let a_triangles = create_attrib_array gl 3 triangles in
  let a_normals = create_attrib_array gl 3 normals in
  let a_shrink_directions = create_attrib_array gl 3 shrink_directions in
  let a_colors = create_attrib_array gl 3 colors in
  let a_border_colors = create_attrib_array gl 3
    (FloatData.init3 (Float32Array.length triangles) (fun _ -> 0.0, 0.0, 0.0))
  in
  object(this)
    inherit identified

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

    method draw shader_id round =
      if shader_id = shader # id && round = 0 then begin

        let x_border, y_border, z_border =
          let x_scale, y_scale, z_scale = scene # scale in
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

    method ray o e = Intersection.ray_triangles triangles table o e

    method magnetize ((x,_,z) as p) =
      match
        match input with
        | `Grid (xs, zs, ys) ->
           let n = Float32Array.length xs in
           let m = Float32Array.length zs in

           assert (n >= 1 && m >= 1);

           let find xs x n =
             let prev = ref (Float32Array.get xs 0) in
             let i = ref 1 in
             while !i < n &&
                   let x' = Float32Array.get xs !i in
                   x' < x && (prev := x'; true)
             do incr i done;
             !i - 1, (!prev +. Float32Array.get xs !i) /. 2.0
           in
           let i, x = find xs x n in
           let j, z = find zs z m in
           if i < n - 1 && j < m - 1 then
             Some [|x; Float32Array.get ys (i * (m - 1) + j); z|]
           else None
        | `List centers ->
          FloatData.closest_point 3 (fun a -> Math.sq (a.(0) -. x) +. Math.sq (a.(2) -. z)) centers
      with Some r -> r.(0), r.(1), r.(2)
         | None -> p

    initializer scene # add (this :> object3d)
  end
