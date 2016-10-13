open Js_array
open Webgl_plot_math
open Webgl_plot_misc
open Webgl_plot_drawable

module Math = Webgl_plot_math
module Geometry = Webgl_plot_geometry
module Shaders = Webgl_plot_shaders
module Intersection = Webgl_plot_intersection
module Repere = Webgl_plot_repere
module Surface = Webgl_plot_surface
module Histogram = Webgl_plot_histogram

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

let colored_sphere gl shader =
  let open Geometry in
  let open Shaders in
  let mesh = Sphere.create 8 in
  let a_positions = create_attrib_array gl 3 mesh.vertices in
  let a_colors_wireframe = create_attrib_array gl 3
    (FloatData.init3 (Float32Array.length mesh.vertices) (fun _ -> 0.0, 0.0, 0.0))
  in
  let e_triangles = create_element_array gl mesh.triangles in
  let e_wireframe = create_element_array gl mesh.wireframe in
  fun color ->
    let a_colors = create_attrib_array gl 3
      (FloatData.init3 (Float32Array.length mesh.vertices) (fun _ -> color));
    in
    object
      inherit dummy_ray
      val mutable scale = (1., 1., 1.)
      val mutable position = (0., 0., 0.)
      method set_scale x = scale <- x
      method set_position x = position <- x
      method draw (_ : context) id =
        if id = shader # id then begin
          shader # set_object_matrix
            (float32_array (Vector.to_array
               (Vector.Const.scale_translation
                  (Vector.of_three scale) (Vector.of_three position))));
          shader # set_colors a_colors;
          shader # set_normals a_positions;
          shader # set_positions  a_positions;
          shader # draw_elements Shaders.Triangles e_triangles;
          shader # set_colors a_colors_wireframe;
          shader # draw_elements Shaders.Lines e_wireframe
        end

    end

type pointer_kind =
  | Cross
  | Sphere
  | None

let prepare_scene gl component =

  let basic_shader = Shaders.Basic.init gl in
  let basic2d_shader = Shaders.Basic2d.init gl in
  let texture_shader = Shaders.Texture.init gl in
  let light_texture_shader = Shaders.LightAndTexture.init gl in

  let repere = Repere.initialize gl texture_shader in

  let sphere_factory = colored_sphere gl basic_shader in
  let textbox = component # new_textbox in
  let sphere_pointer = sphere_factory (0.0, 0.0, 0.0) in
  let () = sphere_pointer # set_scale (0.005, 0.005, 0.005) in

  object(this)
    val mutable aspect = 1.0
    val mutable angle = (0., 0., 0.)
    val mutable move = (0., 0., 0.)
    val mutable pointer = (0., 0.)
    val mutable height = 100
    val mutable width = 100
    val mutable magnetic = false
    val mutable clock = 0.0
    val mutable pointer_kind = Cross

    val mutable objects : #drawable list = []

    method pointer = pointer

    method set_aspect x = aspect <- x
    method set_angle x = angle <- x
    method set_move x = move <- x
    method set_pointer p = pointer <- p
    method set_height h = height <- h
    method set_width w = width <- w
    method set_pointer_kind k = pointer_kind <- k

    method set_magnetic b = magnetic <- b
    method set_clock c = clock <- c

    method repere = repere

    method add_uniform_histogram ?widths ?colors ?wireframe ?name x z y = ()
    method add_parametric_histogram ?widths ?colors ?wireframe ?name a b p = ()
    method add_uniform_scatter ?widths ?colors ?wireframe ?name x z y = ()
    method add_parametric_scatter ?widths ?colors ?wireframe ?name a b p = ()

    method add_uniform_surface ?colors ?wireframe ?name x z y =
      let obj = Surface.create gl light_texture_shader basic2d_shader basic_shader ?name ?colors ?wireframe x z y in
      objects <- (obj :> drawable) :: objects

    method add_parametric_surface ?colors ?wireframe ?name a b p = ()

    method add_histogram xs zs ys =
      let obj = Histogram.create gl basic_shader xs zs ys in
      objects <- (obj :> drawable) :: objects

    method add_sphere position scale color =
      let obj = sphere_factory color in
      obj # set_position position;
      obj # set_scale scale;
      objects <- (obj :> drawable) :: objects

    method render =
      match repere # frame with
      | None -> repere # set_frame
      | Some frame ->
        begin
          print_endline "frame";
          let this = (this :> context) in
          let _proportion, matrix, matrix' = world_matrix aspect frame angle move in
          let flat_matrix = float32_array (Vector.to_array matrix) in

          basic2d_shader # use;
          List.iter (fun o -> o # draw this (basic2d_shader # id)) objects;


          Webgl.(
            bind_framebuffer gl Constant._FRAMEBUFFER_ None;
            enable gl Constant._DEPTH_TEST_;
            viewport gl 0 0 width height);

          texture_shader # use;
          texture_shader # set_world_matrix flat_matrix;
          begin
            let angle_x, angle_y, _ = angle in
            repere # draw angle_x angle_y
          end;

          light_texture_shader # use;
          light_texture_shader # set_world_matrix flat_matrix;
          light_texture_shader # set_ambient_light 1.0 1.0 1.0;
          light_texture_shader # set_light_position 1.0 1.0 (-2.0);
          List.iter (fun o -> o # draw this (light_texture_shader # id)) objects;

          basic_shader # use;
          basic_shader # set_world_matrix flat_matrix;
          basic_shader # set_ambient_light 1.0 1.0 1.0;
          basic_shader # set_light_position 1.0 1.0 (-2.0);
          List.iter (fun o -> o # draw this (basic_shader # id)) objects;

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
                sphere_pointer # draw this (basic_shader # id)
              end
          end
        end
  end

