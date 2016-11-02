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
    bounds: box;
  }

  let create n m vertices =
    let normals =
      compute_normals n m vertices
    in
    let triangles = triangles_indexes_from_grid n m in
    let wireframe = lines_indexes_from_grid n m in
    let bounds = bounding_box vertices in
    {
      triangles;
      wireframe;
      normals;
      vertices;
      bounds
    }

  let contour n m vertices proj =
    (* proj i j should gives the "signed" distance from some oriented plane *)

    let result = ref [] in (* we will resturn a list of segment *)

    (* This function assumes that the point (i1,j1) is in the other side of the plane than (i2,j2) and (s3,j3).
     * In other word, s2 and s3 have the same sign and s1 the other (s1,s2,s3 are the respective distances to the plane).
     * The side effect of the function is to add to the result the the intersection of the triangle formed by the three points
     * and the plane (it's a simple interpolation that is justified by the "intercept theorem" (aka in French "thales").
     * *)
    let add_segment s1 i1 j1 s2 i2 j2 s3 i3 j3 =
      let t2' = s1 /. (s1 -. s2) in
      let t3' = s1 /. (s1 -. s3) in
      let t2 = 1. -. t2' in
      let t3 = 1. -. t3' in
      let pos1 = 3 * (i1 * m + j1) in
      let x1 = Float32Array.get vertices pos1 in
      let y1 = Float32Array.get vertices (pos1 + 1) in
      let z1 = Float32Array.get vertices (pos1 + 2) in
      let pos2 = 3 * (i2 * m + j2) in
      let x2 = Float32Array.get vertices pos2 in
      let y2 = Float32Array.get vertices (pos2 + 1) in
      let z2 = Float32Array.get vertices (pos2 + 2) in
      let pos3 = 3 * (i3 * m + j3) in
      let x3 = Float32Array.get vertices pos3 in
      let y3 = Float32Array.get vertices (pos3 + 1) in
      let z3 = Float32Array.get vertices (pos3 + 2) in
      let a = (x1 *. t2 +. x2 *. t2', y1 *. t2 +. y2 *. t2', z1 *. t2 +. z2 *. t2') in
      let b = (x1 *. t3 +. x3 *. t3', y1 *. t3 +. y3 *. t3', z1 *. t3 +. z3 *. t3') in
      result := a :: b :: !result;
    in
    (* This function test if the triangle is intersecting the plane, and adds the intersection
     * to the result if so. *)
    let test i1 j1 i2 j2 i3 j3 =
      let s1 = proj i1 j1 in
      let s2 = proj i2 j2 in
      let s3 = proj i3 j3 in
      if (s1 < 0.0 && s2 >= 0.0 && s3 >= 0.0) ||
         (s1 > 0.0 && s2 <= 0.0 && s3 <= 0.0) then
        add_segment s1 i1 j1 s2 i2 j2 s3 i3 j3
      else if (s2 < 0.0 && s1 >= 0.0 && s3 >= 0.0) ||
              (s2 > 0.0 && s1 <= 0.0 && s3 <= 0.0) then
        add_segment s2 i2 j2 s1 i1 j1 s3 i3 j3
      else if (s3 < 0.0 && s1 >= 0.0 && s2 >= 0.0) ||
              (s3 > 0.0 && s1 <= 0.0 && s2 <= 0.0) then
        add_segment s3 i3 j3 s1 i1 j1 s2 i2 j2
      else if s1 = 0.0 && s2 = 0.0 && s3 = 0.0 then begin
        (* For flat triangles we take two arbitrary sides: *)
        add_segment 0.0 i1 j1 1.0 i2 j2 1.0 i3 j3;
        add_segment 0.0 i2 j2 1.0 i1 j1 1.0 i3 j3;
      end
    in
    (* We apply the test function on all the "squares" of our mesh: *)
    for i = 0 to n-2 do
      for j = 0 to m-2 do
        test i j (i + 1) j i (j + 1);
        test (i + 1) j (i + 1) (j + 1) i (j + 1);
      done
    done;
    !result

end

class type t =
  object
    inherit object3d

    method set_alpha : float option -> unit
    method set_wireframe: bool -> unit
    method set_magnetic: bool -> unit
    method set_crosshair: bool -> unit

    method x_projection: float -> (float * float) list
    method y_projection: float -> (float * float) list
    method z_projection: float -> (float * float) list

  end


let create (scene : Webgl_plot_scene.scene) ?(name = "") ?(wireframe = false) ?(magnetic = false) ?(crosshair = false) ?colors ?alpha n m vertices : t =
  let open Shaders in

  let gl = scene # gl in

  let shader = scene # basic_shader in

  let min, max =
    let min = ref max_float in
    let max = ref min_float in
    FloatData.update_min_max min max vertices 3 1;
    !min, !max
  in
  let {SurfaceGeometry.vertices; triangles; wireframe = wireframe_vertices; normals; bounds;} =
    SurfaceGeometry.create n m vertices
  in

  let colors = match colors with
    | None ->
      let range = (max -. min) in
      (FloatData.init3 (Float32Array.length vertices) (fun k ->
           let y = Float32Array.get vertices (3 * k + 1) in
           Color.cold_to_hot ((y -. min) /. range)))
    | Some colors -> colors
  in

  let table = if true then Intersection.build_ray_table vertices triangles else [] in
  let a_positions = create_attrib_array gl 3 vertices in
  let a_normals = create_attrib_array gl 3 normals in
  let a_colors_wireframe =
    create_attrib_array gl 3 (* black *)
      (FloatData.init3 (Float32Array.length vertices) (fun _ -> 0.0, 0.0, 0.0))
  in
  let a_colors = create_attrib_array gl 3 colors  in
  let e_triangles = create_element_array gl triangles in
  let e_wireframe = create_element_array gl wireframe_vertices in

  let identity_matrix =
    Float32Array.new_float32_array (`Data [|
         1.0; 0.; 0.; 0.;
         0.0; 1.0; 0.; 0.;
         0.0; 0.0; 1.0; 0.0;
         0.0; 0.0; 0.0; 1.0
      |])
  in

  object(this)
    inherit identified
    inherit with_alpha ?alpha ()

    val name = name
    val mutable wireframe = wireframe

    val mutable magnetic = magnetic
    val grid_width = 0.003

    val mutable crosshair = crosshair

    method hash_state = digest (wireframe, magnetic, crosshair)

    method set_magnetic b = magnetic <- b
    method set_wireframe b = wireframe <- b
    method set_crosshair b = crosshair <- b

    method name = name

    method bounds = bounds

    method draw shader_id round =
      if round >= 0 && (shader_id = shader # id) && ((opaque && round < 2) || (not opaque && round = 2)) then begin
        shader # set_alpha alpha;
        shader # set_explode 0.0;
        shader # set_shrink (0.0, 0.0, 0.0);

        shader # set_object_matrix identity_matrix;
        shader # set_colors a_colors;
        shader # set_normals a_normals;
        shader # set_shrink_directions a_normals;
        shader # set_positions a_positions;
        shader # draw_elements Shaders.Triangles e_triangles;

        if wireframe then begin
          shader # set_alpha alpha;
          shader # set_colors a_colors_wireframe;
          if opaque then begin
            shader # set_explode 0.0001;
            shader # draw_elements Shaders.Lines e_wireframe;
            shader # set_explode (-0.0001);
          end;
          shader # draw_elements Shaders.Lines e_wireframe;
        end
      end

    method ray o e = Intersection.ray_triangles vertices table o e

    method magnetize ((x,y,z) as p) =
      let scale_x, scale_y, scale_z = scene # scale in
      if magnetic then
        match
          FloatData.closest_point 3 (fun a -> Math.sq ((a.(0) -. x) /. scale_x) +. Math.sq ((a.(1) -. y) /. scale_y) +. Math.sq ((a.(2) -. z) /. scale_z)) vertices
        with Some r -> r.(0), r.(1), r.(2)
           | None -> x,y,z
      else p

    method x_projection (x : float) = SurfaceGeometry.contour n m vertices (fun i j -> Float32Array.get vertices (3 * (i * m + j)) -. x) |> List.map (fun (_,y,z) -> (z,y))
    method y_projection (y : float) = SurfaceGeometry.contour n m vertices (fun i j -> Float32Array.get vertices (3 * (i * m + j) + 1) -. y) |> List.map (fun (x,_,z) -> (x,z))
    method z_projection (z : float) = SurfaceGeometry.contour n m vertices (fun i j -> Float32Array.get vertices (3 * (i * m + j) + 2) -. z) |> List.map (fun (x,y,_) -> (x,y))

    initializer scene # add (this :> object3d)

  end
