open Math

module Float32Array = Webgl.Float32Array

let my_projection aspect =
  Vector.Const.projection ~fov:(pi /. 4.0) ~near:0.001 ~far:4.0 ~aspect

let my_inverse_projection aspect =
  Vector.Const.inverse_projection ~fov:(pi /. 4.0) ~near:0.001 ~far:4.0 ~aspect

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

  let matrix =
    translation (of_three (move_x, move_y, move_z))
    |> multiply (scale (of_three (scale_x, scale_y, scale_z)))
    |> multiply (z_rotation angle_z)
    |> multiply (y_rotation angle_y)
    |> multiply (x_rotation angle_x)
    |> multiply (translation (of_three (trans_x, trans_y, trans_z)))
    |> multiply flip
    |> multiply (my_projection aspect)
  in

  let proportions = of_three (1.0 /. scale_x, 1.0 /. scale_y, 1.0 /. scale_z) in

  let matrix' =
    (my_inverse_projection aspect)
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

let colored_sphere gl shader =
  let open Geometry in
  let open Shaders in
  let mesh = Sphere.create 20 in
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
      inherit basic_object gl shader
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
    if range < 1e-10 then
      Vector.to_three (Math.Color.hsv (0.5 *. 359.9) 1.0 1.0)
    else if false then
      Vector.to_three (Math.Color.hsv (359.9 *. (1.0 -. (y -. min) /. range)) 1.0 1.0)
    else
      ((y -. min) /. range, 0.0, 0.0)
  in
  let {Geometry.Surface.vertices; triangles; wireframe; normals} = Geometry.Surface.create xs zs ys in
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
    inherit basic_object gl shader
    method a_colors = a_colors
    method a_normals = a_normals
    method a_colors_wireframe = a_colors_wireframe
    method a_positions = a_positions
    method e_wireframe = e_wireframe
    method e_triangles = e_triangles
  end



let prepare_scene gl =
  let basic_shader = Shaders.Basic.init gl in
  let texture_shader = Shaders.Texture.init gl in
  let repere = Repere.initialize gl texture_shader in
  let sphere_factory = colored_sphere gl basic_shader in
  object
    val mutable aspect = 1.0
    val mutable angle = (0., 0., 0.)
    val mutable move = (0., 0., 0.)

    val mutable objects = []

    method set_aspect x = aspect <- x
    method set_angle x = angle <- x
    method set_move x = move <- x

    method repere = repere

    method add_surface xs zs ys =
      let obj = rainbow_surface gl basic_shader xs zs ys in
      objects <- obj :: objects

    method add_sphere position scale color =
      let obj = sphere_factory color in
      obj # set_position position;
      obj # set_scale scale;
      objects <- obj :: objects

    method render =
      match repere # frame with
      | None -> ()
      | Some frame -> begin
          let _proportion, matrix, _matrix' = world_matrix aspect frame angle move in
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
        end
  end



