open Math

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

class virtual position gl shader =
  object
    inherit Shaders.Basic.drawable gl shader as super
    val mutable scale = (1., 1., 1.)
    val mutable position = (0., 0., 0.)

    method set_scale x =
      scale <- x

    method set_position x =
      position <- x

    method! draw =
      shader # set_object_matrix
        (flatten_matrix
           (Vector.Const.scale_translation
              (Vector.of_three scale) (Vector.of_three position)));
      super # draw
  end

class colored_sphere gl shader color =
  object(this)
    inherit position gl shader
    inherit Geometry.sphere 10
    inherit Geometry.colored color

    initializer
      this # set_colors (this # colors);
      this # set_indexes (this # indexes);
      this # set_positions (this # points);
      this # set_normals (this # normals)
  end

class rainbow_surface gl shader xs zs ys =
  let n = Array.length ys in
  let rainbow (_, y, _) _ =
    if false then
      Vector.to_three (Math.Color.hsv (359.9 *. (y -. ys.(0)) /. (ys.(n-1) -. ys.(0))) 1.0 1.0)
    else
      (0.0, (y -. ys.(0)) /. (ys.(n-1) -. ys.(0)), 0.0)
  in
  object(this)
    inherit position gl shader
    inherit Geometry.surface xs zs ys
    inherit Geometry.colored rainbow

    initializer
      this # set_colors (this # colors);
      this # set_indexes (this # indexes);
      this # set_positions (this # points);
      this # set_normals (this # normals)
  end

let prepare_scene gl =
  let basic_shader = Shaders.Basic.init gl in
  let texture_shader = Shaders.Texture.init gl in
  let repere = Repere.initialize gl texture_shader in
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
      let obj = new rainbow_surface gl basic_shader xs zs ys in
      objects <- obj :: objects

    method add_sphere position scale color =
      let obj = new colored_sphere gl basic_shader (fun _ _ -> color) in
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



