open Js_array
open Webgl_plot_misc
open Webgl_plot_math
open Webgl_plot_drawable

module Geometry = Webgl_plot_geometry
module Shaders = Webgl_plot_shaders
module Intersection = Webgl_plot_intersection

let create gl (shader : Shaders.LightAndTexture.shader) (shader_texture : Shaders.Basic2d.shader) (shader_wireframe : Shaders.Basic.shader)
              ?(name = "") ?(wireframe = false) ?colors ?alpha xs zs ys =
  let open Shaders in
  let min, max = match FloatData.min_max ys with Some c -> c | None -> 0.0, 1.0 in
  let xs = array_of_float32 xs in
  let ys = array_of_float32 ys in
  let zs = array_of_float32 zs in
  let {Geometry.Surface.vertices; triangles; wireframe = wireframe_vertices; normals; bounds; texcoords} =
    Geometry.Surface.create xs zs ys
  in
  let alpha, opaque =
    match alpha with
    | Some alpha -> alpha, false
    | None -> 1.0, true
  in
  let colors = match colors with
    | None ->
      let range = (max -. min) in
      (FloatData.init3 (Float32Array.length vertices) (fun k ->
           let y = Float32Array.get vertices (3 * k + 1) in
           Color.cold_to_hot ((y -. min) /. range)))
    | Some colors -> colors
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
  let table = if true then Intersection.build_ray_table vertices triangles else [] in
  let a_positions = create_attrib_array gl 3 vertices in
  let a_normals = create_attrib_array gl 3 normals in
  let a_colors_wireframe =
    create_attrib_array gl 3 (* black *)
      (FloatData.init3 (Float32Array.length vertices) (fun _ -> 0.0, 0.0, 0.0))
  in
  let a_colors = create_attrib_array gl 3 colors  in
  let a_texcoords = create_attrib_array gl 2 texcoords in
  let e_triangles = create_element_array gl triangles in
  let e_wireframe = create_element_array gl wireframe_vertices in
  let a_grid = new attrib_array gl 3 in
  let a_grid_colors = create_attrib_array gl 3 (FloatData.init3 12 (fun _ -> 0.0, 0.0, 0.0)) in
  let framebuffer = new fbo gl 512 512 in
  object
    val name = name
    val wireframe = wireframe
    val alpha = alpha
    val opaque = opaque
    val texture_resolution = 512


    val mutable last_intersection = None
    val mutable grid_width = 0.005

    method draw (_ctx : context) id round =
      let open Webgl in
      let open Constant in
      if round = 0 && id = shader_texture # id then begin
        framebuffer # bind;

        shader_texture # set_alpha alpha;
        if round = 0 then begin
          shader_texture # set_matrix texture_matrix;
          shader_texture # set_colors a_colors;
          shader_texture # set_positions a_positions;
          shader_texture # draw_elements Shaders.Triangles e_triangles;
        end;

        begin match last_intersection with
        | Some p when round = 0 ->
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

          shader_texture # set_matrix identity_matrix;
          shader_texture # set_colors a_grid_colors;
          shader_texture # set_positions a_grid;
          shader_texture # set_alpha 1.0;
          shader_texture # draw_arrays Shaders.Triangles (a_grid # count);
        | _ -> ()
        end
      end else if (id = shader # id) && ((opaque && round < 2) || (not opaque && round = 2)) then begin
        shader # set_object_matrix identity_matrix;
        shader # set_texcoords a_texcoords;
        shader # set_normals a_normals;
        shader # set_positions a_positions;

        Webgl.bind_texture gl _TEXTURE_2D_ (Some (framebuffer # texture));
        shader # draw_elements Shaders.Triangles e_triangles;
        Webgl.bind_texture gl _TEXTURE_2D_ None

      end else if (id = shader_wireframe # id) && wireframe
               && ((opaque && round = 0)
            || (not opaque && round = 2)) then begin
        shader_wireframe # set_alpha 1.0;
        shader_wireframe # set_object_matrix identity_matrix;
        shader_wireframe # set_positions a_positions;
        shader_wireframe # set_colors a_colors_wireframe;
        shader_wireframe # set_normals a_normals;
        shader_wireframe # draw_elements Shaders.Lines e_wireframe
      end

    method ray o e =
      let r =
        Intersection.ray_triangles vertices table o e
      in
      last_intersection <- r;
      r
  end

