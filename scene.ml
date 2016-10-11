open Math


class type context =
  object
    method pointer : float * float
  end

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
      inherit dummy_ray
      val mutable scale = (1., 1., 1.)
      val mutable position = (0., 0., 0.)
      method set_scale x = scale <- x
      method set_position x = position <- x
      method draw (_ : context) id =
        if id = shader # id then begin
          shader # set_object_matrix
            (flatten_matrix
               (Vector.Const.scale_translation
                  (Vector.of_three scale) (Vector.of_three position)));
          shader # set_colors a_colors;
          shader # set_normals a_positions;
          shader # set_positions  a_positions;
          shader # draw_elements Shaders.Triangles e_triangles;
          shader # set_colors a_colors_wireframe;
          shader # draw_elements Shaders.Lines e_wireframe
        end

    end

let rainbow_surface gl (shader : Shaders.LightAndTexture.shader) (shader2d : Shaders.Basic2d.shader) xs zs ys =
  let open Shaders in
  let min, max = match Array.min_max ys with Some c -> c | None -> 0.0, 1.0 in
  let range = (max -. min) in
  let rainbow y =
      Math.Color.cold_to_hot ((y -. min) /. range)
  in
  let {Geometry.Surface.vertices; triangles; wireframe; normals; bounds; texcoords} =
    Geometry.Surface.create xs zs ys
  in
  let texture_matrix =
    let scale_x, scale_z =
      2.0 /. (bounds.x_max -. bounds.x_min), 2.0 /. (bounds.z_max -. bounds.z_min)
    in
    let shift_x, shift_z =
      -1.0 -. 2.0 *. bounds.x_min /. (bounds.x_max -. bounds.x_min),
      -1.0 -. 2.0 *. bounds.z_min /. (bounds.z_max -. bounds.z_min)
    in
    (* Projects (x,_,z) into [-1,1] × [-1,1] × {1} *)
    Float32Array.new_float32_array (`Data [|
     scale_x; 0.; 0.; 0.;
         0.0; 0.; 0.; 0.;
         0.0; scale_z; 0.0; 0.0;
     shift_x; shift_z; 1.0; 1.0
      |])
  in
  let identity_matrix =
    Float32Array.new_float32_array (`Data [|
         1.0; 0.; 0.; 0.;
         0.0; 1.0; 0.; 0.;
         0.0; 0.0; 1.0; 0.0;
         0.0; 0.0; 0.0; 1.0
      |])
  in
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
  let a_texcoords = create_attrib_array gl 2 texcoords in
  let e_triangles = create_element_array gl triangles in
  let e_wireframe = create_element_array gl wireframe in
  let texture_framebuffer = Webgl.create_framebuffer gl in
  let texture_surface = Webgl.create_texture gl in
  let a_grid = new attrib_array gl 3 in
  let a_grid_colors = create_attrib_array gl 3 (Geometry.init_triple_array 12 (fun _ -> 0.0, 0.0, 0.0)) in
  object
    val mutable last_intersection = None
    val mutable grid_width = 0.005

    method draw (ctx : context) id =
      let open Webgl in
      let open Constant in
      if id = shader2d # id then begin
        bind_framebuffer gl _FRAMEBUFFER_ (Some texture_framebuffer);
        disable gl _DEPTH_TEST_;

        viewport gl 0 0 1024 1024;
        shader2d # set_matrix texture_matrix;
        shader2d # set_colors a_colors;
        shader2d # set_positions a_positions;
        shader2d # draw_elements Shaders.Triangles e_triangles;

        begin match last_intersection with
        | None -> ()
        | Some p ->
          let x,_,z = Vector.to_three p in
          let x = -1. +. 2.0 *. (x -. bounds.x_min) /. (bounds.x_max -. bounds.x_min) in
          let z = -1. +. 2.0 *. (z -. bounds.z_min) /. (bounds.z_max -. bounds.z_min) in

          a_grid # fill (Float32Array.new_float32_array (`Data [|
              x -. grid_width;-1.0; 1.0;
              x +. grid_width;-1.0; 1.0;
              x -. grid_width; 1.0; 1.0;
              x +. grid_width;-1.0; 1.0;
              x +. grid_width; 1.0; 1.0;
              x -. grid_width; 1.0; 1.0;
              -1.0;z -. grid_width; 1.0;
              -1.0;z +. grid_width; 1.0;
               1.0;z -. grid_width; 1.0;
              -1.0;z +. grid_width; 1.0;
               1.0;z +. grid_width; 1.0;
               1.0;z -. grid_width; 1.0;
            |]));

          shader2d # set_matrix identity_matrix;
          shader2d # set_colors a_grid_colors;
          shader2d # set_positions a_grid;
          shader2d # draw_arrays Shaders.Triangles (a_grid # count);
        end;

        bind_texture gl _TEXTURE_2D_ (Some texture_surface);
        generate_mipmap gl _TEXTURE_2D_;
        bind_texture gl _TEXTURE_2D_ None;
      end else if id = shader # id then begin
        shader # set_object_matrix identity_matrix;
        shader # set_texcoords a_texcoords;
        shader # set_normals a_normals;
        shader # set_positions  a_positions;
        Webgl.bind_texture gl _TEXTURE_2D_ (Some texture_surface);
        shader # draw_elements Shaders.Triangles e_triangles;
        Webgl.bind_texture gl _TEXTURE_2D_ None;
      end
      (*
      shader # set_colors a_colors_wireframe;
      shader # draw_elements Shaders.Lines e_wireframe *)

    method ray o e =
      let r =
        Intersection.ray_triangles vertices table o e
      in
      last_intersection <- r;
      r

    initializer
      let open Webgl in
      let open Webgl.Constant in
      bind_texture gl _TEXTURE_2D_ (Some texture_surface);
      tex_image_2D_array gl _TEXTURE_2D_ 0 _RGBA_ 1024 1024 0 _RGBA_ _UNSIGNED_BYTE_  None;
      tex_parameteri gl _TEXTURE_2D_ _TEXTURE_MAG_FILTER_ _LINEAR_;
      tex_parameteri gl _TEXTURE_2D_ _TEXTURE_MIN_FILTER_ _LINEAR_MIPMAP_LINEAR_;
      bind_framebuffer gl _FRAMEBUFFER_ (Some texture_framebuffer);
      framebuffer_texture_2D gl _FRAMEBUFFER_ _COLOR_ATTACHMENT0_ _TEXTURE_2D_ texture_surface 0;
      bind_framebuffer gl _FRAMEBUFFER_ None;
      bind_texture gl _TEXTURE_2D_ None

  end

let histogram gl (shader : Shaders.Basic.shader) xs zs ys =
  let open Shaders in
  let open Geometry in
  let min, max = match Array.min_max ys with Some c -> c | None -> 0.0, 1.0 in
  let range = max -. min in
  let rainbow y =
      Math.Color.white_cold_to_hot ((y -. min) /. range)
  in
  let {Histogram.triangles; normals; wireframe; normals_wireframe} = Histogram.create xs zs ys in
  let a_triangles = create_attrib_array gl 3 triangles in
  let a_normals = create_attrib_array gl 3 normals in
  let a_colors = create_attrib_array gl 3 (* rainbow *)
    (Geometry.init_triple_array (Float32Array.length triangles) (fun k ->
        rainbow (Float32Array.get triangles (3 * k + 1))))
  in
  let a_wireframe = create_attrib_array gl 3 wireframe in
  let a_normals_wireframe = create_attrib_array gl 3 normals_wireframe in
  let a_colors_wireframe =
    create_attrib_array gl 3 (* black *)
      (Geometry.init_triple_array (Float32Array.length triangles) (fun _ -> 0.0, 0.0, 0.0))
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
          (flatten_matrix
             (Vector.Const.scale_translation
                (Vector.of_three scale) (Vector.of_three position)));
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


class type drawable =
  object
    method draw : context -> int -> unit
    method ray: three Vector.vector -> three Vector.vector -> three Vector.vector option
  end

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

    val mutable objects : #drawable list = []

    method pointer = pointer

    method set_aspect x = aspect <- x
    method set_angle x = angle <- x
    method set_move x = move <- x
    method set_pointer p = pointer <- p
    method set_height h = height <- h
    method set_width w = width <- w

    method repere = repere

    method add_surface xs zs ys =
      let obj = rainbow_surface gl light_texture_shader basic2d_shader xs zs ys in
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
          let this = (this :> context) in
          let _proportion, matrix, matrix' = world_matrix aspect frame angle move in
          let flat_matrix = flatten_matrix matrix in

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

