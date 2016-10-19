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

let world_matrix aspect {Geometry.x_max; x_min; y_max; y_min; z_min; z_max} (angle_x, angle_y, angle_z) (trans_x, trans_y, trans_z) (ratio_x, ratio_y, ratio_z) =
  let open Vector in
  let open Const in
  let range_x = x_max -. x_min in
  let range_y = y_max -. y_min in
  let range_z = z_max -. z_min in

  let move_x = -. range_x /. 2.0 -. x_min in
  let move_y = -. range_y /. 2.0 -. y_min in
  let move_z = -. range_z /. 2.0 -. z_min in

  let scale_x = ratio_x /. range_x in
  let scale_y = ratio_y /. range_y in
  let scale_z = ratio_z /. range_z in

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

  let matrix' =
    (my_inverse_projection near far aspect)
    |> multiply flip
    |> multiply (translation (of_three (-. trans_x, -. trans_y, -. trans_z)))
    |> multiply (x_rotation (-. angle_x))
    |> multiply (y_rotation (-. angle_y))
    |> multiply (z_rotation (-. angle_z))
    |> multiply (scale (of_three (1.0 /. scale_x, 1.0 /. scale_y, 1.0 /. scale_z)))
    |> multiply (translation
                   (of_three (-. move_x, -. move_y, -. move_z)))
  in
  matrix, matrix'

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
      method opaque = true
      method set_scale x = scale <- x
      method set_position x = position <- x
      method draw (_ : context) id round =
        if id = shader # id && round = 0 then begin
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

  let screen_shader = Shaders.Screen.init gl in
  let basic_shader = Shaders.Basic.init gl in
  let basic2d_shader = Shaders.Basic2d.init gl in
  let repere_shader = Shaders.Texture.init gl in
  let light_texture_shader = Shaders.LightAndTexture.init gl in

  let repere = Repere.initialize gl repere_shader in

  let sphere_factory = colored_sphere gl basic_shader in
  let textbox = component # new_textbox in
  let sphere_pointer = sphere_factory (0.0, 0.0, 0.0) in

  let composite_layer = new Shaders.fbo gl 1024 1024 in
  object(this)
    val mutable aspect = 1.0
    val mutable angle = (0., 0., 0.)
    val mutable move = (0., 0., 0.)
    val mutable pointer = (0., 0.)
    val mutable height = 100
    val mutable width = 100
    val mutable clock = 0.0
    val mutable pointer_kind = Cross

    val mutable pointer_projection = (0., 0., 0.)
    val mutable selected_object : #drawable option = None
    val mutable pointer_magnetic = (0., 0., 0.)

    val mutable objects : #drawable list = []

    val sphere_size = 0.01

    method pointer = pointer
    method pointer_projection = pointer_projection
    method projection_valid = selected_object <> None

    method set_aspect x = aspect <- x
    method set_angle x = angle <- x
    method set_move x = move <- x
    method set_pointer p = pointer <- p
    method set_height h = height <- max h 1
    method set_width w = width <- max w 1
    method set_pointer_kind k = pointer_kind <- k
    method set_clock c = clock <- c

    method repere = repere

    method scale = repere # scale

    method add_uniform_histogram ?widths ?depths ?colors ?name ?border x z y =
      let obj = Histogram.create gl basic_shader ?name ?widths ?depths ?colors ?border (`Grid (x, z, y)) in
      objects <- (obj :> drawable) :: objects

    method add_list_histogram ?widths ?depths ?colors ?name ?border centers =
      let obj = Histogram.create gl basic_shader ?name ?widths ?depths ?colors ?border (`List centers) in
      objects <- (obj :> drawable) :: objects

    method add_uniform_scatter ?radius ?colors ?name x z y =
      ignore (x, z, y, colors, radius, name)

    method add_parametric_scatter ?radius ?colors ?name x z y =
      ignore (x, z, y, colors, radius, name)

    method add_uniform_surface ?colors ?wireframe ?name ?alpha ?magnetic x z y =
      let obj = Surface.create gl light_texture_shader basic2d_shader basic_shader ?name ?colors ?wireframe ?alpha ?magnetic ~parametric:false x z y in
      objects <- (obj :> drawable) :: objects

    method add_parametric_surface ?colors ?wireframe ?name ?alpha ?magnetic x z y =
      let obj = Surface.create gl light_texture_shader basic2d_shader basic_shader ?name ?colors ?wireframe ?alpha ?magnetic ~parametric:true x z y in
      objects <- (obj :> drawable) :: objects

    method add_sphere position scale color =
      let obj = sphere_factory color in
      obj # set_position position;
      obj # set_scale scale;
      objects <- (obj :> drawable) :: objects

    method render =
      let frame = repere # box in
      begin
        let context = (this :> context) in
        let open Webgl in
        let open Constant in
        let matrix, matrix' = world_matrix aspect frame angle move (repere # ratio) in
        let flat_matrix = float32_array (Vector.to_array matrix) in

        depth_mask gl true;
        disable gl _BLEND_;
        enable gl _DEPTH_TEST_;
        color_mask gl true true true true;
        clear gl (_DEPTH_BUFFER_BIT_ lor _COLOR_BUFFER_BIT_);

        repere_shader # use;
        repere_shader # set_world_matrix flat_matrix;
        begin
          let angle_x, angle_y, _ = angle in
          repere # draw angle_x angle_y
        end;

        repere_shader # switch;

        (* round :
         * 0 - all opaque things
         * ----- in framebuffer
         * 1 - redraw opaque, only depth
         * 2 - draw all transparent things with blending
         *
         * At the end we display the framebuffer on the screen shader
         * *)

        let max_round = if List.for_all (fun x -> x # opaque) objects then 0 else 2 in

        for round = 0 to max_round do

          if round = 0 then begin
            basic2d_shader # use;
            List.iter (fun o -> o # draw context (basic2d_shader # id) round) objects;
            basic2d_shader # switch;
            bind_framebuffer gl _FRAMEBUFFER_ None;
            viewport gl 0 0 width height;
          end;

          if round = 1 then begin
            composite_layer # resize width height;
            composite_layer # bind;
            clear_color gl 0.0 0.0 0.0 0.0;
            clear gl (_DEPTH_BUFFER_BIT_ lor _COLOR_BUFFER_BIT_);
            depth_mask gl true;
            color_mask gl false false false false;
          end;

          if round = 2 then begin
            depth_mask gl false;
            color_mask gl true true true true;
            enable gl _BLEND_;
            blend_func gl _SRC_ALPHA_ _ONE_MINUS_DST_ALPHA_;
            clear_color gl 0.0 0.0 0.0 0.0;
            clear gl _COLOR_BUFFER_BIT_
          end;

          light_texture_shader # use;
          light_texture_shader # set_world_matrix flat_matrix;
          light_texture_shader # set_ambient_light 1.0 1.0 1.0;
          light_texture_shader # set_light_position 1.0 1.0 (-2.0);
          List.iter (fun o -> o # draw context (light_texture_shader # id) round) objects;
          light_texture_shader # switch;

          basic_shader # use;
          basic_shader # set_world_matrix flat_matrix;
          basic_shader # set_ambient_light 1.0 1.0 1.0;
          basic_shader # set_light_position 1.0 1.0 (-2.0);
          List.iter (fun o -> o # draw context (basic_shader # id) round) objects;
          if selected_object <> None then begin
            let x_scale, y_scale, z_scale = repere # scale in
            sphere_pointer # set_scale (sphere_size *. x_scale, sphere_size *.y_scale, sphere_size *. z_scale);
            sphere_pointer # set_position pointer_magnetic;
            sphere_pointer # draw context (basic_shader # id) 0;
          end;
          basic_shader # switch;
        done;

        bind_framebuffer gl _FRAMEBUFFER_ None;
        viewport gl 0 0 width height;
        enable gl _DEPTH_TEST_;
        depth_mask gl true;

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
          match List.choose (fun obj ->
             match obj # ray o e with
             | Some p ->
               let x,y,z = Vector.to_three p in
               let q =
                 multiply_vector matrix (Vector.of_four (x,y,z, 1.0))
               in
               let _,_,d,w = Vector.to_four q in
               Some (d /. w, p, obj)
             | None -> None) objects |> List.sort (fun (d, _, _) (d', _, _) -> compare d d') with
          | [] -> begin
              selected_object <- None;
              textbox # set_text "";
              component # set_cursor_visibility true;
            end
          | (_, p, obj) :: _ -> begin
              textbox # set_position pointer;
              component # set_cursor_visibility false;
              pointer_projection <- Vector.to_three p;
              pointer_magnetic <- obj # magnetize pointer_projection;
              let (x,y,z) = pointer_magnetic in
              textbox # set_text (Printf.sprintf "%.2f, %.2f, %.2f" x y z);
              selected_object <- Some obj;
            end
        end;

        if max_round = 2 then begin
          screen_shader # use;
          bind_texture gl _TEXTURE_2D_ (Some (composite_layer # texture));
          disable gl _DEPTH_TEST_;
          enable gl _BLEND_;
          blend_func gl _ONE_ _ONE_MINUS_SRC_ALPHA_;
          screen_shader # draw;
          screen_shader # switch;
        end

      end

    initializer
      let open Webgl in
      let open Constant in
      depth_func gl _LEQUAL_

  end

