open Math

module Float32Array = Webgl.Float32Array

let my_projection near far aspect =
  Vector.Const.projection ~fov:(pi /. 4.0) ~near ~far ~aspect

let my_inverse_projection near far aspect =
  Vector.Const.inverse_projection ~fov:(pi /. 4.0) ~near ~far ~aspect

let world_matrix aspect {Geometry.x_max; x_min; y_max; y_min; z_min; z_max} (angle_x, angle_y, angle_z) (trans_x, trans_y, trans_z) =
  let open Vector in
  let open Const in
  let range_x = x_max -. x_min in
  let range_y = y_max -. y_min in
  let range_z = z_max -. z_min in

  let move_x = -. range_x /. 2.0 -. x_min in
  let move_y = -. range_y /. 2.0 -. y_min in
  let move_z = -. range_z /. 2.0 -. z_min in

  let scale_x = 1.0 /. range_x in
  let scale_y = 1.0 /. range_y in
  let scale_z = 1.0 /. range_z in

  let near, far =
     max 0.1 (-1. +. trans_z), 2.0 +. trans_z
  in

  let matrix =
    translation (of_three (move_x, move_y, move_z))
    |> multiply (scale (of_three (scale_x, scale_y, scale_z)))
    |> multiply (z_rotation angle_z)
    |> multiply (y_rotation angle_y)
    |> multiply (x_rotation angle_x)
    |> multiply (translation (of_three (trans_x, trans_y, trans_z)))
    |> multiply flip
    |> multiply (my_projection near far aspect)
  in

  let proportions = of_three (1.0 /. scale_x, 1.0 /. scale_y, 1.0 /. scale_z) in

  let matrix' =
    (my_inverse_projection near far aspect)
    |> multiply flip
    |> multiply (translation (of_three (-. trans_x, -. trans_y, -. trans_z)))
    |> multiply (x_rotation (-. angle_x))
    |> multiply (y_rotation (-. angle_y))
    |> multiply (z_rotation (-. angle_z))
    |> multiply (scale proportions)
    |> multiply (translation
                   (of_three (-. move_x, -. move_y, -. move_z)))
  in
  proportions, matrix, matrix'

let flatten_matrix m =
  Webgl.Float32Array.new_float32_array (`Data (Vector.to_array m))

class virtual basic_object _gl (shader : Shaders.Basic.shader)  =
  object(this)
    method virtual a_triangles : Shaders.attrib_array
    method virtual a_colors : Shaders.attrib_array
    method virtual a_normals : Shaders.attrib_array

    method virtual a_wireframe : Shaders.attrib_array
    method virtual a_colors_wireframe : Shaders.attrib_array
    method virtual a_normals_wireframe : Shaders.attrib_array

    val mutable scale = (1., 1., 1.)
    val mutable position = (0., 0., 0.)

    method set_scale x =
      scale <- x

    method set_position x =
      position <- x

    method draw =
      shader # set_object_matrix
        (flatten_matrix
           (Vector.Const.scale_translation
              (Vector.of_three scale) (Vector.of_three position)));
      shader # set_colors (this # a_colors);
      shader # set_normals (this # a_normals);
      shader # set_positions (this # a_triangles);
      shader # draw_arrays Shaders.Triangles (this # a_triangles # count);
      shader # set_positions (this # a_wireframe);
      shader # set_colors (this # a_colors_wireframe);
      shader # set_normals (this # a_normals_wireframe);
      shader # draw_arrays Shaders.Lines (this # a_wireframe # count)
  end


class virtual basic_indexed_object _gl (shader : Shaders.Basic.shader)  =
  object(this)
    method virtual a_positions : Shaders.attrib_array
    method virtual a_colors : Shaders.attrib_array
    method virtual a_normals : Shaders.attrib_array
    method virtual e_triangles : Shaders.element_array
    method virtual e_wireframe : Shaders.element_array
    method virtual a_colors_wireframe : Shaders.attrib_array

    val mutable scale = (1., 1., 1.)
    val mutable position = (0., 0., 0.)

    method set_scale x =
      scale <- x

    method set_position x =
      position <- x

    method draw =
      shader # set_object_matrix
        (flatten_matrix
           (Vector.Const.scale_translation
              (Vector.of_three scale) (Vector.of_three position)));
      shader # set_colors (this # a_colors);
      shader # set_normals (this # a_normals);
      shader # set_positions (this # a_positions);
      shader # draw_elements Shaders.Triangles (this # e_triangles);
      shader # set_colors (this # a_colors_wireframe);
      shader # draw_elements Shaders.Lines (this # e_wireframe)
  end

class dummy_ray = object
  method ray (_ : three Vector.vector) (_ : three Vector.vector) = (None : three Vector.vector option)
end

let colored_sphere gl shader =
  let open Geometry in
  let open Shaders in
  let mesh = Sphere.create 8 in
  let a_positions = create_attrib_array gl 3 mesh.vertices in
  let a_colors_wireframe = create_attrib_array gl 3
    (Geometry.init_triple_array (Float32Array.length mesh.vertices) (fun _ -> 0.0, 0.0, 0.0))
  in
  let e_triangles = create_element_array gl mesh.triangles in
  let e_wireframe = create_element_array gl mesh.wireframe in
  fun color ->
    let a_colors = create_attrib_array gl 3
      (Geometry.init_triple_array (Float32Array.length mesh.vertices) (fun _ -> color));
    in
    object
      inherit basic_indexed_object gl shader
      inherit dummy_ray
      method a_colors = a_colors
      method a_normals = a_positions
      method a_colors_wireframe = a_colors_wireframe
      method a_positions = a_positions
      method e_wireframe = e_wireframe
      method e_triangles = e_triangles
    end

let rainbow_surface gl shader xs zs ys =
  let open Shaders in
  let min, max = match Array.min_max ys with Some c -> c | None -> 0.0, 1.0 in
  let range = (max -. min) in
  let rainbow y =
      Math.Color.cold_to_hot ((y -. min) /. range)
  in
  let {Geometry.Surface.vertices; triangles; wireframe; normals} = Geometry.Surface.create xs zs ys in
  let table = Intersection.build_ray_table vertices triangles in
  let a_positions = create_attrib_array gl 3 vertices in
  let a_normals = create_attrib_array gl 3 normals in
  let a_colors_wireframe =
    create_attrib_array gl 3 (* black *)
      (Geometry.init_triple_array (Float32Array.length vertices) (fun _ -> 0.0, 0.0, 0.0))
  in
  let a_colors = create_attrib_array gl 3 (* rainbow *)
    (Geometry.init_triple_array (Float32Array.length vertices) (fun k ->
        rainbow (Float32Array.get vertices (3 * k + 1))))
  in
  let e_triangles = create_element_array gl triangles in
  let e_wireframe = create_element_array gl wireframe in
  object
    inherit basic_indexed_object gl shader
    method a_colors = a_colors
    method a_normals = a_normals
    method a_colors_wireframe = a_colors_wireframe
    method a_positions = a_positions
    method e_wireframe = e_wireframe
    method e_triangles = e_triangles
    method ray o e = Intersection.ray_triangles vertices table o e
  end

let histogram gl shader xs zs ys =
  let open Shaders in
  let open Geometry in
  let min, max = match Array.min_max ys with Some c -> c | None -> 0.0, 1.0 in
  let range = max -. min in
  let rainbow y =
      Math.Color.white_cold_to_hot ((y -. min) /. range)
  in
  let {Histogram.triangles; normals; wireframe; wireframe_normals} = Histogram.create xs zs ys in
  let a_triangles = create_attrib_array gl 3 triangles in
  let a_normals = create_attrib_array gl 3 normals in
  let a_colors = create_attrib_array gl 3 (* rainbow *)
    (Geometry.init_triple_array (Float32Array.length triangles) (fun k ->
        rainbow (Float32Array.get triangles (3 * k + 1))))
  in
  let a_wireframe = create_attrib_array gl 3 wireframe in
  let a_wireframe_normals = create_attrib_array gl 3 wireframe_normals in
  let a_colors_wireframe =
    create_attrib_array gl 3 (* black *)
      (Geometry.init_triple_array (Float32Array.length triangles) (fun _ -> 0.0, 0.0, 0.0))
  in
  object
    inherit basic_object gl shader
    inherit dummy_ray

    method a_triangles = a_triangles
    method a_colors = a_colors
    method a_normals = a_normals

    method a_colors_wireframe = a_colors_wireframe
    method a_normals_wireframe = a_wireframe_normals
    method a_wireframe = a_wireframe
  end

class type drawable =
  object
    method draw : unit
    method ray: three Vector.vector -> three Vector.vector -> three Vector.vector option
  end

let prepare_scene gl component =
  let basic_shader = Shaders.Basic.init gl in
  let texture_shader = Shaders.Texture.init gl in
  let repere = Repere.initialize gl texture_shader in
  let sphere_factory = colored_sphere gl basic_shader in
  let textbox = component # new_textbox in
  let sphere_pointer = sphere_factory (0.0, 0.0, 0.0) in
  let () = sphere_pointer # set_scale (0.005, 0.005, 0.005) in
  object
    val mutable aspect = 1.0
    val mutable angle = (0., 0., 0.)
    val mutable move = (0., 0., 0.)
    val mutable pointer = (0., 0.)

    val mutable objects : #drawable list = []

    method set_aspect x = aspect <- x
    method set_angle x = angle <- x
    method set_move x = move <- x
    method set_pointer p = pointer <- p

    method repere = repere

    method add_surface xs zs ys =
      let obj = rainbow_surface gl basic_shader xs zs ys in
      objects <- (obj :> drawable) :: objects

    method add_histogram xs zs ys =
      let obj = histogram gl basic_shader xs zs ys in
      objects <- (obj :> drawable) :: objects

    method add_sphere position scale color =
      let obj = sphere_factory color in
      obj # set_position position;
      obj # set_scale scale;
      objects <- (obj :> drawable) :: objects

    method render =
      match repere # frame with
      | None -> ()
      | Some frame -> begin
          let _proportion, matrix, matrix' = world_matrix aspect frame angle move in

          texture_shader # use;
          texture_shader # set_world_matrix (flatten_matrix matrix);
          begin
            let angle_x, angle_y, _ = angle in
            repere # draw angle_x angle_y
          end;

          basic_shader # use;
          basic_shader # set_world_matrix (flatten_matrix matrix);
          basic_shader # set_ambient_light 1.0 1.0 1.0;
          basic_shader # set_light_position 1.0 1.0 (-2.0);

          List.iter (fun o -> o # draw) objects;

          let x,y = pointer in
          begin
            let open Math.Vector in
            let o =
              four_to_three
                  (multiply_vector matrix' (Vector.of_four (x,y,1.0,1.0)))
              in
              let e =
                four_to_three
                  (multiply_vector matrix' (Vector.of_four (x,y,-1.0,1.0)))
              in
              match List.choose (fun x -> x # ray o e) objects with
              | [] -> begin
                  textbox # set_text "";
                  component # set_cursor_visibility true;
                end
              | p :: _ -> begin
                  textbox # set_position pointer;
                  component # set_cursor_visibility false;
                  let (x,y,z) as p = Vector.to_three p in
                  textbox # set_text (Printf.sprintf "%.2f, %.2f, %.2f" x y z);
                  sphere_pointer # set_position p;
                  sphere_pointer # draw
                end
            end
        end
  end



