(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

open Js_array
open Webgl_plot_misc
open Webgl_plot_math
open Webgl_plot_drawable

module Math = Webgl_plot_math
module Shaders = Webgl_plot_shaders
module Intersection = Webgl_plot_intersection
module Geometry = Webgl_plot_geometry

module HistogramGeometry = struct
  open Geometry

  type t = {
    normals: Float32Array.t;
    triangles: Float32Array.t;
    colors: Float32Array.t;
    shrink_directions: Float32Array.t;
  }

  (* Invariant: forall i j, length (f i j) = dim *)
  let flatten dim n m f =
    let points = Float32Array.new_float32_array (`Size (n * m * dim)) in
    let pos = ref 0 in
    for i = 0 to n - 1 do
      for j = 0 to m - 1 do
        let a = f i j in
        assert (Array.length a = dim);
        Array.iteri (fun k x ->
            Float32Array.set points (!pos + k) x) a;
        pos := !pos + dim;
      done
    done;
    points

  let create ?widths ?depths ?colors input =
    let n, m =
      match input with
      | `Grid (xs, zs, ys) ->
        let n, m = Float32Array.length xs - 1, Float32Array.length zs - 1 in
        assert (Float32Array.length ys = n * m);
        n, m
      | `List centers -> Float32Array.length centers / 3, 1
    in
    let get_w = match widths with
      | None -> fun _ _ -> 0.95
      | Some w -> fun i j -> Float32Array.get w (i * m + j)
    in
    let get_h = match depths with
      | None -> fun _ _ -> 0.95
      | Some h ->
        fun i j -> Float32Array.get h (i * m + j)
    in
    let has_box =
      match input with
      | `Grid (_,_,ys) ->
        fun i j ->
          let y = Float32Array.get ys (i * m + j) in
          classify_float y <> FP_nan
      | _ -> fun _ _ -> true
    in
    let get_box =
      match input with
      | `List (centers) ->
        fun i j ->
          let pos = 3 * (i * m + j) in
          let x = Float32Array.get centers pos in
          let y = Float32Array.get centers (pos + 1) in
          let z = Float32Array.get centers (pos + 2) in
          let w = get_w i j in
          let h = get_h i j in
          let x_min = x -. 0.5 *. w in
          let x_max = x +. 0.5 *. w in
          let z_min = z -. 0.5 *. h in
          let z_max = z +. 0.5 *. h in
          let y_min = 0.0 in
          let y_max = y in
          {x_min; x_max; z_min; z_max; y_min;y_max}
      | `Grid (xs,zs,ys) ->
        fun i j ->
          let y_max = Float32Array.get ys (i * m + j) in
          let y_min = 0.0 in
          let x_min = Float32Array.get xs i in
          let x_max = Float32Array.get xs (i+1) in
          let z_min = Float32Array.get zs j in
          let z_max = Float32Array.get zs (j+1) in
          let w = get_w i j in
          let h = get_h i j in
          let x_min, x_max =
            let mid = x_min +. x_max in
            let dif = x_max -. x_min in
            (mid -. w *. dif) /. 2.0,
            (mid +. w *. dif) /. 2.0
          in
          let z_min, z_max =
            let mid = z_min +. z_max in
            let dif = z_max -. z_min in
            (mid -. h *. dif) /. 2.0,
            (mid +. h *. dif) /. 2.0
          in
          {x_min; x_max; z_min; z_max; y_min;y_max}
    in
    let get_color = match colors with
      | None ->
        let min, max, get_y =
          match input with
          | `List centers ->
            let get_y i _ =
              Float32Array.get centers (3 * i + 1)
            in
            if Float32Array.length centers > 1 then begin
              let min = ref (Float32Array.get centers 1) in
              let max = ref !min in
              for k = 1 to (Float32Array.length centers) / 3 - 1 do
                let y = Float32Array.get centers (3 * k + 1) in
                if y < !min then min := y;
                if y > !max then max := y;
              done;
              (!min, !max, get_y)
            end else (0.0, 1.0, get_y)
          | `Grid (_,_,ys) ->
            let get_y i j =
              Float32Array.get ys (i * m + j)
            in
            match FloatData.min_max ys with Some (min, max) -> (min, max, get_y) | None -> 0.0, 1.0, get_y
        in
        let range = max -. min in
        fun i j ->
          let y = get_y i j in
          let x, y, z = Color.white_cold_to_hot ((y -. min) /. range) in
          [|x;y;z|]

      | Some color -> fun i j ->
        let pos = i * m + j in
        let x = Float32Array.get color pos in
        let y = Float32Array.get color (pos + 1) in
        let z = Float32Array.get color (pos + 2) in
        [|x;y;z|]
    in
    let dim = 6 * 2 * 3 * 3 in
    let triangles =
      flatten dim n m
        (fun i j ->
           if has_box i j then
             let {x_min; x_max; z_min; z_max; y_min;y_max} = get_box i j in
             let v1 = [| x_min; y_max; z_min|] in
             let v2 = [| x_max; y_max; z_min|] in
             let v3 = [| x_max; y_max; z_max|] in
             let v4 = [| x_min; y_max; z_max|] in
             let v5 = [| x_min; y_min; z_max|] in
             let v6 = [| x_max; y_min; z_max|] in
             let v7 = [| x_max; y_min; z_min|] in
             let v8 = [| x_min; y_min; z_min|] in
             Array.concat [
               v1;v2;v3;v3;v4;v1;
               v5;v6;v7;v7;v8;v5;
               v2;v7;v6;v6;v3;v2;
               v1;v4;v5;v5;v8;v1;
               v3;v6;v5;v5;v4;v3;
               v1;v8;v7;v7;v2;v1;
             ]
           else [||])
    in
    let normals =
      flatten dim n m
        (fun i j ->
           if has_box i j then
             let {y_min;y_max; _} = get_box i j in
             let top = [| 0.; 1.; 0.|] in
             let bot = [| 0.; -1.; 0.|] in
             let top, bot = if y_min < y_max then top, bot else bot, top in
             let right = [| 1.; 0.; 0.|] in
             let left = [| -1.; 0.; 0.|] in
             let back = [| 0.; 0.; 1.|] in
             let front = [| 0.; 0.; -1.|] in
             Array.concat [
               top; top; top;
               top; top; top;
               bot; bot; bot;
               bot; bot; bot;
               right; right; right;
               right; right; right;
               left; left; left;
               left; left; left;
               back; back; back;
               back; back; back;
               front; front; front;
               front; front; front;
             ]
           else [||])
    in
    let colors =
      flatten dim n m
        (fun i j ->
           if has_box i j then begin
             let a = get_color i j in
             let r = Array.create_float dim in
             for k = 0 to dim-1 do
               r.(k) <- a.(k mod 3);
             done;
             r
           end else [||])
    in
    let shrink_directions =
      flatten dim n m
        (fun i j ->
           if has_box i j then
             let {y_min;y_max; _} = get_box i j in
             let top, bot = if y_min < y_max then 1., -1. else -1., 1. in
             let left_front = [| -1.; 0.; -1.|] in
             let right_front = [| 1.; 0.; -1.|] in
             let right_back = [| 1.; 0.; 1.|] in
             let left_back = [| -1.; 0.; 1.|] in

             let top_front = [| 0.; top; -1.|] in
             let bot_front = [| 0.; bot; -1.|] in
             let top_back = [| 0.; top; 1.|] in
             let bot_back = [| 0.; bot; 1.|] in

             let left_top = [| -1.; top; 0.|] in
             let right_top = [| 1.; top; 0.|] in
             let right_bot = [| 1.; bot; 0.|] in
             let left_bot = [| -1.; bot; 0.|] in

             Array.concat [
               left_front; right_front; right_back; right_back; left_back; left_front;
               left_back; right_back; right_front; right_front; left_front; left_back;
               top_front; bot_front; bot_back; bot_back; top_back; top_front;
               top_front; top_back; bot_back; bot_back; bot_front; top_front;
               right_top; right_bot; left_bot; left_bot; left_top; right_top;
               left_top; left_bot; right_bot; right_bot; right_top; left_top;
             ]
           else [||])
    in
    {
      triangles;
      normals;
      shrink_directions;
      colors
    }
end

class type t = object
  inherit object3d

  method set_alpha : float option -> unit
  method set_border : float -> unit
end

let create (scene : Webgl_plot_scene.scene) ?(name = "") ?widths ?depths ?colors ?(border = 0.001) input : t =
  let open Shaders in
  let gl = scene # gl in
  let shader = scene # basic_shader in
  let {HistogramGeometry.triangles; normals; shrink_directions; colors} =
    HistogramGeometry.create ?widths ?depths ?colors input
  in
  let indexes = Index.of_array (Array.init ((Float32Array.length triangles) / 3) (fun x -> x)) in
  let table = if true then Intersection.build_ray_table triangles indexes else [] in
  let a_triangles = create_attrib_array gl 3 triangles in
  let a_normals = create_attrib_array gl 3 normals in
  let a_shrink_directions = create_attrib_array gl 3 shrink_directions in
  let a_colors = create_attrib_array gl 3 colors in
  let a_border_colors = create_attrib_array gl 3
    (FloatData.init3 (Float32Array.length triangles) (fun _ -> 0.0, 0.0, 0.0))
  in
  object(this)
    inherit identified
    inherit with_alpha ()

    val name = name
    val scale = (1., 1., 1.)
    val position = (0., 0., 0.)

    val mutable border = border

    method hash_state = digest border

    method set_border x =
      border <- (x /. 1000.0)

    method name = name

    method draw shader_id round =
      if shader_id = shader # id && round >= 0 && round <= 1 then begin

        let x_scale, y_scale, z_scale = scene # scale in
        let x_border, y_border, z_border =
          -. border *. x_scale,
          -. border *. y_scale,
          -. border *. z_scale
        in
        shader # set_alpha alpha;
        shader # set_object_matrix
          (float32_array (Vector.to_array
             (Vector.Const.scale_translation
                (Vector.of_three scale) (Vector.of_three position))));
        shader # set_normals a_normals;
        shader # set_shrink_directions a_shrink_directions;
        shader # set_positions a_triangles;
        shader # set_colors a_border_colors;
        shader # set_shrink (0.0, 0.0, 0.0);
        shader # set_explode (0.0, 0.0, 0.0);
        shader # draw_arrays Shaders.Triangles (a_triangles # count);
        shader # set_shrink (x_border, y_border, z_border);
        shader # set_explode (0.0001 *. x_scale, 0.0001 *. y_scale, 0.0001 *. z_scale);
        shader # set_colors a_colors;
        shader # draw_arrays Shaders.Triangles (a_triangles # count);
      end

    method bounds = Geometry.bounding_box triangles

    method ray o e = Intersection.ray_triangles triangles table o e

    method magnetize ((x,_,z) as p) =
      match
        match input with
        | `Grid (xs, zs, ys) ->
           let n = Float32Array.length xs in
           let m = Float32Array.length zs in

           assert (n >= 1 && m >= 1);

           let find xs x n =
             let prev = ref (Float32Array.get xs 0) in
             let i = ref 1 in
             while !i < n &&
                   let x' = Float32Array.get xs !i in
                   x' < x && (prev := x'; true)
             do incr i done;
             !i - 1, (!prev +. Float32Array.get xs !i) /. 2.0
           in
           let i, x = find xs x n in
           let j, z = find zs z m in
           if i < n - 1 && j < m - 1 then
             Some [|x; Float32Array.get ys (i * (m - 1) + j); z|]
           else None
        | `List centers ->
          let scale_x, _, scale_z = scene # scale in
          FloatData.closest_point 3 (fun a -> Math.sq ((a.(0) -. x) /. scale_x) +. Math.sq ((a.(2) -. z) /. scale_z)) centers
      with Some r -> r.(0), r.(1), r.(2)
         | None -> p

    initializer scene # add (this :> object3d)
  end
