(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

open Js_array
open Webgl_plot_math
open Webgl_plot_misc

let normal_of_triangle a b c =
  let open Vector in
  let x1, y1, z1 = to_three a in
  let x2, y2, z2 = to_three b in
  let x3, y3, z3 = to_three c in
  let xn = (y2 -. y1) *. (z3 -. z1) -. (z2 -. z1) *. (y3 -. y1) in
  let yn = (z2 -. z1) *. (x3 -. x1) -. (x2 -. x1) *. (z3 -. z1) in
  let zn = (x2 -. x1) *. (y3 -. y1) -. (y2 -. y1) *. (x3 -. x1) in
  let n = sqrt (xn *. xn +. yn *. yn +. zn *. zn) in
  (of_three (xn /. n, yn /. n, zn /. n))

let compute_vertices dim1 dim2 desc =
  let n, m = Array.length dim1, Array.length dim2 in
  FloatData.init3_matrix n m (fun i j -> desc dim1.(i) dim2.(j))

let texcoords_from_grid n m =
  let n' = float (n - 1) in
  let m' = float (m - 1) in
  FloatData.init2_matrix n m (fun i j -> 0.001 +. (float i) /. n', (float j) /. m' -. 0.001)

let compute_normals n m points =
  FloatData.init3_matrix n m (fun i j ->
      let get i j =
        let pos = (i*m + j) * 3 in
        Vector.of_three
          (Float32Array.get points pos,
           Float32Array.get points (pos + 1), Float32Array.get points (pos+2))
      in
      let res = ref (Vector.of_three (0., 0., 0.)) in
      let cpt = ref 0.0 in
      let o = get i j in
      let add_one i1 j1 i2 j2 =
        let a = get i1 j1 in
        let b = get i2 j2 in
        res := Vector.add !res (normal_of_triangle o b a);
        cpt := !cpt +. 1.0;
      in
      (*     1
       *    /|\
       *   4-O-2
       *    \|/
       *     3
       **)
      if 0 < j && i < n-1 then
        add_one i (j-1) (i+1) j; (* 1 - 2 *)
      if j < m-1 && i < n-1 then
        add_one (i+1) j i (j+1); (* 2 - 3 *)
      if 0 < i && j < m-1 then
        add_one i (j+1) (i-1) j; (* 3 - 4 *)
      if 0 < i && 0 < j then
        add_one (i-1) j i (j-1); (* 4 - 1 *)

      Vector.scale (1.0 /. !cpt) !res |> Vector.to_three)

let triangles_indexes_from_grid dim1 dim2 =
  let aux result set =
    let k = ref 0 in
    for i = 0 to dim1 - 2 do
      for j = 0 to dim2 - 2 do
        set result !k (i * dim2 + j); incr k;
        set result !k ((i + 1) * dim2 + j); incr k;
        set result !k (i * dim2 + j + 1); incr k;
        set result !k ((i + 1) * dim2 + j); incr k;
        set result !k ((i + 1) * dim2 + j + 1); incr k;
        set result !k (i * dim2 + j + 1); incr k;
      done
    done
  in
  let size = dim1 * dim2 * 2 * 3 in
  if size < 256 then
    let result = Uint8Array.new_uint8_array (`Size size) in
    aux result Uint8Array.set;
    `Byte result
  else if size < 65536 then
    let result = Uint16Array.new_uint16_array (`Size size) in
    aux result Uint16Array.set;
    `Short result
  else
    let result = Uint32Array.new_uint32_array (`Size size) in
    aux result Uint32Array.set;
    `Int result

type box = {
  x_min : float;
  x_max : float;
  y_min : float;
  y_max : float;
  z_min : float;
  z_max : float;
}

let neutral_box = {
  x_min = min_float;
  x_max = max_float;
  y_min = min_float;
  y_max = max_float;
  z_min = min_float;
  z_max = max_float;
}

let merge_box b1 b2 = {
  x_min = min b1.x_min b2.x_min;
  x_max = max b1.x_max b2.x_max;
  y_min = min b1.y_min b2.y_min;
  y_max = max b1.y_max b2.y_max;
  z_min = min b1.z_min b2.z_min;
  z_max = max b1.z_max b2.z_max;
}


let bounding_box points =
  if Float32Array.length points mod 3 <> 0 then
    failwith "bounding_box: input should an array of length divisible by 3"
  else if Float32Array.length points < 3 then
    failwith "bounding_box: not enough points"
  else
    let x_min, x_max =
      let x = Float32Array.get points 0 in
      ref x, ref x
    in
    let y_min, y_max =
      let y = Float32Array.get points 1 in
      ref y, ref y
    in
    let z_min, z_max =
      let z = Float32Array.get points 2 in
      ref z, ref z
    in
    FloatData.iteri (fun k v ->
        let r_min, r_max = match k mod 3 with 0 -> x_min, x_max | 1 -> y_min, y_max | 2 -> z_min, z_max | _ -> assert false in
        if !r_min > v then r_min := v;
        if !r_max < v then r_max := v;
      ) points;
    {
      x_min = !x_min;
      x_max = !x_max;
      y_min = !y_min;
      y_max = !y_max;
      z_min = !z_min;
      z_max = !z_max
    }

let uniform_array res min max =
  Array.init res (fun i -> min +. (float i) *. (max -. min) /. (float (res - 1)))

let lines_indexes_from_grid dim1 dim2 =
  let size = 2 * (dim1 * (dim2 - 1) + dim2 * (dim1 - 1)) in
  let segments = Array.make size 0 in
  let k = ref 0 in
  let idx i j = i * dim2 + j in
  for i = 0 to dim1 - 1 do
    for j = 0 to dim2 - 2 do
      segments.(!k) <- idx i j; incr k;
      segments.(!k) <- idx i (j + 1); incr k;
    done
  done;
  for j = 0 to dim2 - 1 do
    for i = 0 to dim1 - 2 do
      segments.(!k) <- idx i j; incr k;
      segments.(!k) <- idx (i + 1) j; incr k;
    done
  done;
  Index.of_array segments

module Sphere = struct
  type t = {
    vertices: Float32Array.t;
    wireframe : Index.t;
    triangles: Index.t;
  }

  let create res =
    let dim1 = uniform_array res 0.0 (2.0 *. pi) in
    let dim2 = uniform_array res 0.0 pi in
    let vertices = compute_vertices dim1 dim2
        (fun theta phi ->
           (cos theta *. sin phi, sin theta *. sin phi, cos phi))
    in
    let triangles = triangles_indexes_from_grid res res in
    let wireframe = lines_indexes_from_grid res res in
    {
      triangles;
      wireframe;
      vertices;
    }
end
