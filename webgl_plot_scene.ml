(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)


open Js_array
open Webgl_plot_math
open Webgl_plot_misc
open Webgl_plot_drawable

module Math = Webgl_plot_math
module Geometry = Webgl_plot_geometry
module Shaders = Webgl_plot_shaders
module Intersection = Webgl_plot_intersection

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
      inherit not_intersectable
      inherit identified

      val mutable scale = (1., 1., 1.)
      val mutable position = (0., 0., 0.)
      val mutable wireframe = false

      method hash_state = digest (scale, position, wireframe)

      method name = "pointer"
      method opaque = true

      method set_scale x = scale <- x
      method set_position x = position <- x
      method set_wireframe b = wireframe <- b

      method bounds = Geometry.neutral_box
      method draw id round =
        if round >= 0 && id = shader # id && round <= 1 then begin
          shader # set_object_matrix
            (float32_array (Vector.to_array
               (Vector.Const.scale_translation
                  (Vector.of_three scale) (Vector.of_three position))));
          shader # set_alpha 1.0;
          shader # set_colors a_colors;
          shader # set_normals a_positions;
          shader # set_positions  a_positions;
          shader # draw_elements Shaders.Triangles e_triangles;
          if wireframe then begin
            shader # set_colors a_colors_wireframe;
            shader # draw_elements Shaders.Lines e_wireframe
          end
        end

    end

class type scene =
  object
    method add : Webgl_plot_drawable.object3d -> unit
    method angle : float * float * float
    method basic2d_shader : Shaders.Basic2d.shader
    method basic_shader : Shaders.Basic.shader
    method frame : Geometry.box
    method gl : Webgl.context
    method light_texture_shader : Shaders.LightAndTexture.shader
    method move : float * float * float
    method bounds: Geometry.box
    method pointer : float * float
    method pointer_magnetic : float * float * float
    method pointer_projection : float * float * float
    method projection_valid : bool
    method ratio : float * float * float
    method hash_state : string
    method render : unit
    method repere_shader : Shaders.Texture.shader
    method scale : float * float * float
    method screen_shader : Shaders.Screen.shader
    method selected : Webgl_plot_drawable.object3d option
    method set_angle : float * float * float -> unit
    method set_aspect : float -> unit
    method set_clock : float -> unit
    method set_frame : Geometry.box -> unit
    method set_height : int -> unit
    method set_move : float * float * float -> unit
    method set_pointer : float * float -> unit
    method set_ratio : float * float * float -> unit
    method set_width : int -> unit
    method pointer_text_formatter : Js_browser.Element.t -> unit
    method set_pointer_text_formatter : (Js_browser.Element.t -> unit) -> unit
    method post_render_hook : (unit -> unit)
    method pre_render_hook : (unit -> unit)
    method set_post_render_hook : (unit -> unit) -> unit
    method set_pre_render_hook : (unit -> unit) -> unit
  end

let prepare_scene gl component : scene =
  let screen_shader = Shaders.Screen.init gl in
  let basic_shader = Shaders.Basic.init gl in
  let basic2d_shader = Shaders.Basic2d.init gl in
  let repere_shader = Shaders.Texture.init gl in
  let light_texture_shader = Shaders.LightAndTexture.init gl in


  let sphere_factory = colored_sphere gl basic_shader in
  let textbox = component # new_textbox in
  let sphere_pointer = sphere_factory (0.0, 0.0, 0.0) in

  let composite_layer = new Shaders.fbo gl 1024 1024 in
  object(this)
    (* Gl and Shaders *)
    method gl = gl
    method repere_shader = repere_shader
    method basic_shader = basic_shader
    method basic2d_shader = basic2d_shader
    method light_texture_shader = light_texture_shader
    method screen_shader = screen_shader

    (* Time *)
    val mutable clock = 0.0
    method set_clock c = clock <- c

    (* Screen *)
    val mutable aspect = 1.0
    val mutable height = 100
    val mutable width = 100

    method set_height h = height <- max h 1
    method set_width w = width <- max w 1
    method set_aspect x = aspect <- x

    (* View *)
    val mutable angle = (0., 0., 0.)
    val mutable move = (0., 0., 0.)
    val mutable frame = {Geometry.x_min = 0.0; x_max = 0.0; y_min = 0.0; y_max = 0.0; z_min = 0.0; z_max = 0.0}
    val mutable ratio = (1.0, 1.0, 1.0)

    method angle = angle
    method move = move
    method frame = frame
    method ratio = ratio

    method scale =
      let {Geometry.x_min; x_max; y_min; y_max; z_min; z_max} = this # frame in
      let x_ratio, y_ratio, z_ratio = ratio in
      (x_max -. x_min) /. x_ratio,
      (y_max -. y_min) /. y_ratio,
      (z_max -. z_min) /. z_ratio

    method set_angle x = angle <- x
    method set_move x = move <- x
    method set_ratio r = ratio <- r
    method set_frame f = frame <- f

    (* Selection *)
    val mutable selected_object : #object3d option = None
    method selected = selected_object

    (* Pointer (mouse) *)
    val mutable pointer = (0., 0.)
    val pointer_size = 0.01
    val mutable pointer_projection = (0., 0., 0.)
    val mutable pointer_magnetic = (0., 0., 0.)

    method pointer = pointer
    method pointer_projection = pointer_projection
    method pointer_magnetic = pointer_magnetic
    method projection_valid = selected_object <> None

    method set_pointer p = pointer <- p

    (* Hooks *)

    val mutable pointer_text_formatter = ignore
    method pointer_text_formatter = pointer_text_formatter
    method set_pointer_text_formatter f = pointer_text_formatter <- f

    val mutable pre_render_hook = ignore
    val mutable post_render_hook = ignore

    method pre_render_hook = pre_render_hook
    method post_render_hook = post_render_hook
    method set_pre_render_hook f = pre_render_hook <- f
    method set_post_render_hook f = post_render_hook <- f

    (* Object *)
    val mutable objects : object3d list = []

    method add (obj : object3d) = objects <- obj :: objects

    method hash_state =
      digest (pointer_magnetic, pointer_projection, pointer, aspect, height, width, angle, move, frame, ratio, List.map (fun o -> o # hash_state) objects)

    method bounds =
      List.fold_left (fun acc o -> Geometry.merge_box acc (o # bounds)) Geometry.neutral_box objects

    method private add_sphere position scale color =
      let obj = sphere_factory color in
      obj # set_position position;
      obj # set_scale scale;
      objects <- (obj :> object3d) :: objects

    val mutable previous_state = ""
    method render =
      let state = this # hash_state in
      if state <> previous_state then begin
        this # render_now;
        previous_state <- state
      end


    method private render_now =
      let open Webgl in
      let open Constant in

      pre_render_hook ();

      let matrix, matrix' = world_matrix aspect frame angle move ratio in
      let flat_matrix = float32_array (Vector.to_array matrix) in

      depth_mask gl true;
      disable gl _BLEND_;
      enable gl _DEPTH_TEST_;
      color_mask gl true true true true;
      clear gl (_DEPTH_BUFFER_BIT_ lor _COLOR_BUFFER_BIT_);

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
          bind_framebuffer gl _FRAMEBUFFER_ None;
          viewport gl 0 0 width height;
        end;

        if round = 0 then begin
          repere_shader # use;
          repere_shader # set_world_matrix flat_matrix;
          List.iter (fun o -> o # draw (repere_shader # id) round) objects;
          repere_shader # switch;
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

        basic2d_shader # use;
        List.iter (fun o -> o # draw (basic2d_shader # id) round) objects;
        basic2d_shader # switch;

        light_texture_shader # use;
        light_texture_shader # set_world_matrix flat_matrix;
        light_texture_shader # set_ambient_light 1.0 1.0 1.0;
        light_texture_shader # set_light_position 1.0 1.0 (-2.0);
        List.iter (fun o -> o # draw (light_texture_shader # id) round) objects;
        light_texture_shader # switch;

        basic_shader # use;
        basic_shader # set_world_matrix flat_matrix;
        basic_shader # set_ambient_light 1.0 1.0 1.0;
        basic_shader # set_light_position 1.0 1.0 (-2.0);
        List.iter (fun o -> o # draw (basic_shader # id) round) objects;
        if selected_object <> None then begin
          let x_scale, y_scale, z_scale = this # scale in
          sphere_pointer # set_scale (pointer_size *. x_scale, pointer_size *.y_scale, pointer_size *. z_scale);
          sphere_pointer # set_position pointer_magnetic;
          sphere_pointer # draw (basic_shader # id) round;
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
            component # set_cursor_visibility true;
          end
        | (_, p, obj) :: _ -> begin
            pointer_projection <- Vector.to_three p;
            if component # mod_down then
              pointer_magnetic <- pointer_projection
            else
              pointer_magnetic <- obj # magnetize pointer_projection;
            let (x,y,z) = pointer_magnetic in
            let q =
              multiply_vector matrix (Vector.of_four (x,y,z, 1.0))
            in
            selected_object <- Some obj;
            let x,y, _ = Vector.to_three (Vector.four_to_three q) in
            textbox # set_position (x,y);
            let x',y' = pointer in
            component # set_cursor_visibility (Math.sq (x -. x') +. Math.sq (y -. y') > 0.001);
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
      end;

      pointer_text_formatter (textbox # element);
      post_render_hook ()

    initializer

      (* The pointer_text_formatter : *)
      pointer_text_formatter <- begin fun element ->
        let open Js_browser in
        let x, y, z = this # pointer_magnetic in
        if this # selected <> None then
          Element.set_text_content element (Printf.sprintf "%.2f, %.2f, %.2f" x y z)
        else
          Element.set_text_content element ""
      end;
      let open Webgl in
      let open Constant in
      depth_func gl _LEQUAL_

  end
