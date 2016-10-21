(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

open Js_array
open Webgl_plot_misc
open Webgl_plot_math
open Webgl_plot_drawable

module Math = Webgl_plot_math
module Geometry = Webgl_plot_geometry
module Shaders = Webgl_plot_shaders
module Intersection = Webgl_plot_intersection


module SurfaceGeometry = struct
  open Geometry

  type t = {
    vertices: Float32Array.t;
    normals: Float32Array.t;
    wireframe : Index.t;
    triangles: Index.t;
    texcoords: Float32Array.t;
    bounds: box;
  }

  let create parametric xs zs ys =
    let n = Float32Array.length xs in
    let m = Float32Array.length zs in
    let vertices =
      if parametric then
        FloatData.init3_matrix n m
          (fun i j ->
             let pos = (i * m + j) * 3 in
             Float32Array.get ys pos,
             Float32Array.get ys (pos + 1),
             Float32Array.get ys (pos + 2))
      else
        FloatData.init3_matrix n m
          (fun i j ->
             Float32Array.get xs i,
             Float32Array.get ys (i * m +j),
             Float32Array.get zs j)
    in
    let normals =
      compute_normals n m vertices
    in
    let triangles = triangles_indexes_from_grid n m in
    let wireframe = lines_indexes_from_grid n m in
    let texcoords = texcoords_from_grid n m in
    let bounds = bounding_box vertices in
    {
      triangles;
      wireframe;
      normals;
      vertices;
      texcoords;
      bounds
    }
end


let create (scene : Webgl_plot_scene.scene) ?(name = "") ?(wireframe = false) ?(magnetic = false) ?colors ?alpha ~parametric xs zs ys =
  let open Shaders in

  let gl = scene # gl in
  let shader = scene # light_texture_shader in
  let shader_texture = scene # basic2d_shader in
  let shader_wireframe = scene # basic_shader in

  let min, max = match FloatData.min_max ys with Some c -> c | None -> 0.0, 1.0 in

  let {SurfaceGeometry.vertices; triangles; wireframe = wireframe_vertices; normals; bounds; texcoords} =
    SurfaceGeometry.create parametric xs zs ys
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
    let x_min, x_max = default_option (0.0, 1.0) (FloatData.min_max xs) in
    let z_min, z_max = default_option (0.0, 1.0) (FloatData.min_max zs) in
    let scale_x, scale_z =
      2.0 /. (x_max -. x_min), 2.0 /. (z_max -. z_min)
    in
    let shift_x, shift_z =
      -1.0 -. 2.0 *. x_min /. (x_max -. x_min),
      -1.0 -. 2.0 *. z_min /. (z_max -. z_min)
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
  let a_params =
    if parametric then
      let n = Float32Array.length xs in
      let m = Float32Array.length zs in
      create_attrib_array gl 3 (FloatData.init3_matrix n m (fun i j ->
         Float32Array.get xs i, 0.0,
         Float32Array.get zs j))
    else
      a_positions
  in
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
  let texture_resolution = 1024 in
  let framebuffer = new fbo gl texture_resolution texture_resolution in

  object(this)
    inherit identified

    val name = name
    val wireframe = wireframe
    val alpha = alpha
    val opaque = opaque

    val mutable magnetic = magnetic
    val mutable grid_width = 0.003

    method name = name
    method opaque = opaque

    method draw shader_id round =
      let open Webgl in
      let open Constant in
      if round = 0 && shader_id = shader_texture # id then begin
        framebuffer # bind;

        shader_texture # set_alpha alpha;
        if round = 0 then begin
          shader_texture # set_matrix texture_matrix;
          shader_texture # set_colors a_colors;
          shader_texture # set_positions a_params;
          shader_texture # draw_elements Shaders.Triangles e_triangles;

          if scene # projection_valid && not parametric && match scene # selected with Some obj -> obj # id = id | _ -> false then begin
            let x,_,z = scene # pointer_magnetic in
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
          end
        end
      end else if (shader_id = shader # id) && ((opaque && round < 2) || (not opaque && round = 2)) then begin
        shader # set_object_matrix identity_matrix;
        shader # set_texcoords a_texcoords;
        shader # set_normals a_normals;
        shader # set_positions a_positions;

        Webgl.bind_texture gl _TEXTURE_2D_ (Some (framebuffer # texture));
        shader # draw_elements Shaders.Triangles e_triangles;
        Webgl.bind_texture gl _TEXTURE_2D_ None

      end else if (shader_id = shader_wireframe # id) && wireframe
               && ((opaque && round = 0)
            || (not opaque && round = 2)) then begin
        shader_wireframe # set_alpha 1.0;
        shader_wireframe # set_explode 0.0;
        shader_wireframe # set_shrink (0.0, 0.0, 0.0);
        shader_wireframe # set_object_matrix identity_matrix;
        shader_wireframe # set_positions a_positions;
        shader_wireframe # set_colors a_colors_wireframe;
        shader_wireframe # set_normals a_normals;
        shader_wireframe # set_shrink_directions a_normals;
        shader_wireframe # draw_elements Shaders.Lines e_wireframe
      end

    method ray o e = Intersection.ray_triangles vertices table o e

    method magnetize ((x,y,z) as p) =
      if magnetic then
        match
          FloatData.closest_point 3 (fun a -> Math.sq (a.(0) -. x) +. Math.sq (a.(1) -. y) +. Math.sq (a.(2) -. z)) vertices
        with Some r -> r.(0), r.(1), r.(2)
           | None -> x,y,z
      else p

    method z_projection z =
      if parametric then
        None
      else
        let n = Float32Array.length xs in
        let m = Float32Array.length zs in
        let res = Float32Array.new_float32_array (`Size m) in
        let j =
           let j = ref 0 in
           while
             !j < m && Float32Array.get zs !j < z
           do incr j done;
           !j
        in
        if j = 0 || j = m then
          None
        else begin
          let j' = j - 1 in
          let t' =
            let z' = Float32Array.get zs j' in
            (z -. z') /. (Float32Array.get zs j -. z')
          in
          let t = 1.0 -. t' in
          for i = 0 to n - 1 do
            let v =
              (Float32Array.get zs (i * m + j')) *. t'
              +. (Float32Array.get zs (i * m + j)) *. t
            in
            Float32Array.set res i v
          done;
          Some (zs, res)
        end



    method x_projection x =
      if parametric then
        None
      else
        let n = Float32Array.length xs in
        let m = Float32Array.length zs in
        let res = Float32Array.new_float32_array (`Size m) in
        let i =
           let i = ref 0 in
           while
             !i < n && Float32Array.get xs !i < x
           do incr i done;
           !i
        in
        if i = 0 || i = n then
          None
        else begin
          let i' = i - 1 in
          let t' =
            let x' = Float32Array.get xs i' in
            (x -. x') /. (Float32Array.get xs i -. x')
          in
          let t = 1.0 -. t' in
          for j = 0 to m - 1 do
            let v =
              (Float32Array.get zs (i' * m + j)) *. t'
              +. (Float32Array.get zs (i * m + j)) *. t
            in
            Float32Array.set res j  v
          done;
          Some (xs, res)
        end

    initializer scene # add (this :> object3d)

  end
