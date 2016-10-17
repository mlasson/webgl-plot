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
    let correct r_min r_max =
      if !r_max -. !r_min < 1e-13 then begin
        r_max := !r_max -. 1.0;
        r_min := !r_min +. 1.0;
      end
    in
    correct x_min x_max;
    correct y_min y_max;
    correct z_min z_max;
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

module Surface = struct

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

module Histogram = struct

  type t = {
    normals: Float32Array.t;
    triangles: Float32Array.t;
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

  let create ?widths ?depths xs zs ys =
    let n, m = Array.length xs, Array.length zs in
    assert (Array.length ys = (n - 1) * (m - 1));
    let get_w = match widths with
      | None -> fun _ _ -> 1.0
      | Some w -> fun i j -> Float32Array.get w (i * (m - 1) + j)
    in
    let get_h = match ignore(depths); widths with
      | None -> fun _ _ -> 1.0
      | Some h -> fun i j -> Float32Array.get h (i * (m - 1) + j)
    in
    let triangles_boxes w h =
      flatten (6 * 2 * 3 * 3) (n-1) (m-1)
        (fun i j ->
           let w = w i j in
           let h = h i j in
           let y_max = ys.(i * (m - 1) + j) in
           let y_min = 0.0 in
           let x_min = xs.(i) in
           let x_max = xs.(i+1) in
           let z_min = zs.(j) in
           let z_max = zs.(j+1) in
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
           let v1 = [| x_min; y_max; z_min|] in
           let v2 = [| x_max; y_max; z_min|] in
           let v3 = [| x_max; y_max; z_max|] in
           let v4 = [| x_min; y_max; z_max|] in
           let v5 = [| x_min; y_min; z_max|] in
           let v6 = [| x_max; y_min; z_max|] in
           let v7 = [| x_max; y_min; z_min|] in
           let v8 = [| x_min; y_min; z_min|] in
           let extrude v = List.map (Array.map2 (+.) v) in
           Array.concat [
              v1;v2;v3;v3;v4;v1;
              v5;v6;v7;v7;v8;v5;
              v2;v7;v6;v6;v3;v2;
              v1;v4;v5;v5;v8;v1;
              v3;v6;v5;v5;v4;v3;
              v1;v8;v7;v7;v2;v1;
           ])
    in
    let triangles = triangles_boxes get_w get_h in
    let normals =
      flatten (6 * 2 * 3 * 3) (n-1) (m-1)
        (fun _ _ ->
           let top = [| 0.; 1.; 0.|] in
           let bot = [| 0.; -1.; 0.|] in
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
           ])
    in
    let shrink_directions =
      flatten (6 * 2 * 3 * 3) (n-1) (m-1)
        (fun _ _ ->
           let left_front = [| -1.; 0.; -1.|] in
           let right_front = [| 1.; 0.; -1.|] in
           let right_back = [| 1.; 0.; 1.|] in
           let left_back = [| -1.; 0.; 1.|] in

           let top_front = [| 0.; 1.; -1.|] in
           let bot_front = [| 0.; -1.; -1.|] in
           let top_back = [| 0.; 1.; 1.|] in
           let bot_back = [| 0.; -1.; 1.|] in

           let left_top = [| -1.; 1.; 0.|] in
           let right_top = [| 1.; 1.; 0.|] in
           let right_bot = [| 1.; -1.; 0.|] in
           let left_bot = [| -1.; -1.; 0.|] in

           Array.concat [
             left_front; right_front; right_back; right_back; left_back; left_front;
             left_back; right_back; right_front; right_front; left_front; left_back;
             top_front; bot_front; bot_back; bot_back; top_back; top_front;
             top_front; top_back; bot_back; bot_back; bot_front; top_front;
             right_top; right_bot; left_bot; left_bot; left_top; right_top;
             left_top; left_bot; right_bot; right_bot; right_top; left_top;
           ])
    in
    {
      triangles;
      normals;
      shrink_directions
    }


end

