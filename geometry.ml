open Math
open Webgl

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

let flatten3 n m f =
  let points = Float32Array.new_float32_array (`Size (n * m * 3)) in
  let pos = ref 0 in
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
       let x, y, z = f i j in
       Float32Array.set points (!pos + 0) x;
       Float32Array.set points (!pos + 1) y;
       Float32Array.set points (!pos + 2) z;
       pos := !pos + 3;
    done
  done;
  points

let flatten2 n m f =
  let points = Float32Array.new_float32_array (`Size (n * m * 2)) in
  let pos = ref 0 in
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
       let x, y = f i j in
       Float32Array.set points (!pos + 0) x;
       Float32Array.set points (!pos + 1) y;
       pos := !pos + 2;
    done
  done;
  points

let compute_vertices dim1 dim2 desc =
  let n, m = Array.length dim1, Array.length dim2 in
  flatten3 n m (fun i j -> desc dim1.(i) dim2.(j))

let texcoords_from_grid n m =
  let n' = float (n - 1) in
  let m' = float (m - 1) in
  flatten2 n m (fun i j -> (float i) /. n', (float j) /. m')

let compute_normals n m points =
  flatten3 n m (fun i j ->
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
        res := Vector.add !res (normal_of_triangle o a b);
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
      if j < m && i < n-1 then
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

module Index = struct

  type t =
    [ `Byte of Uint8Array.t
    | `Short of Uint16Array.t
    | `Int of Uint32Array.t]

  let length = function
     | `Int data -> Uint32Array.length data
     | `Short data -> Uint16Array.length data
     | `Byte data -> Uint8Array.length data

  let get = function
     | `Int data -> Uint32Array.get data
     | `Short data -> Uint16Array.get data
     | `Byte data -> Uint8Array.get data

  let set = function
     | `Int data -> Uint32Array.set data
     | `Short data -> Uint16Array.set data
     | `Byte data -> Uint8Array.set data

  let of_array a : t =
    let len = Array.length a in
    if len < 256 then
      `Byte (Uint8Array.new_uint8_array (`Data a))
    else if len < 65536 then
      `Short (Uint16Array.new_uint16_array (`Data a))
    else
      `Int (Uint32Array.new_uint32_array (`Data a))


end

module Buffer = struct
  open Webgl

  let iteri f a =
    let open Float32Array in
    for k = 0 to length a - 1 do
       f k (get a k)
    done

  let iter_generic dim float_array f =
    let tmp = Array.create_float dim in
    iteri (fun k x ->
        let i = k mod dim in
        tmp.(i) <- x;
        if i = dim - 1 then
          f tmp
      ) float_array

  let vec3_of_array a =
    match Vector.of_array a with
    | `Three v -> v
    | _ -> failwith "vec3_of_array"

  let iter3 buffer f =
    iter_generic 3 buffer (fun a ->
        f (vec3_of_array (Array.copy a)))

  let number_of_triangles indexes =
    Index.length indexes / 3

  let iter_triangles indexes f =
    let size = number_of_triangles indexes in
    let get = Index.get indexes in
    for k = 0 to size - 1 do
        f (get (3 * k),
           get (3 * k + 1),
           get (3 * k + 2))
    done


  let get_generic tmp float_array k =
    let dim = Array.length tmp in
    for i = 0 to dim - 1 do
      tmp.(i) <- Float32Array.get float_array (k * dim + i)
    done

  let get3 buffer k =
    let tmp = Array.create_float 3 in
    get_generic tmp buffer k;
    vec3_of_array tmp
end

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
    Buffer.iteri (fun k v ->
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

  let create xs zs ys =
  let n, m = Array.length xs, Array.length zs in
  let vertices =
    flatten3 n m
      (fun i j ->
         (xs.(i), ys.(i*m + j), zs.(j)))
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
    triangles: Float32Array.t;
    normals: Float32Array.t;
    wireframe: Float32Array.t;
    normals_wireframe: Float32Array.t;
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

  let create xs zs ys =
    let n, m = Array.length xs, Array.length zs in
    assert (Array.length ys = (n - 1) * (m - 1));
    let triangles =
      flatten (6 * 2 * 3 * 3) (n-1) (m-1)
        (fun i j ->
           let y_max = ys.(i * (m - 1) + j) in
           let y_min = 0.0 in
           let x_min = xs.(i) in
           let x_max = xs.(i+1) in
           let z_min = zs.(j) in
           let z_max = zs.(j+1) in
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
           ])
    in
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
    let wireframe =
      flatten (6 * 2 * 4 * 3) (n-1) (m-1)
        (fun i j ->
           let y_max = ys.(i * (m - 1) + j) in
           let y_min = 0.0 in
           let x_min = xs.(i) in
           let x_max = xs.(i+1) in
           let z_min = zs.(j) in
           let z_max = zs.(j+1) in
           let v1 = [| x_min; y_max; z_min|] in
           let v2 = [| x_max; y_max; z_min|] in
           let v3 = [| x_max; y_max; z_max|] in
           let v4 = [| x_min; y_max; z_max|] in
           let v5 = [| x_min; y_min; z_max|] in
           let v6 = [| x_max; y_min; z_max|] in
           let v7 = [| x_max; y_min; z_min|] in
           let v8 = [| x_min; y_min; z_min|] in
           Array.concat [
             v1;v2;v2;v3;v3;v4;v4;v1;
             v5;v6;v6;v7;v7;v8;v8;v5;
             v2;v7;v7;v6;v6;v3;v3;v2;
             v1;v4;v4;v5;v5;v8;v8;v1;
             v3;v6;v6;v5;v5;v4;v4;v3;
             v1;v8;v8;v7;v7;v2;v2;v1;
           ])
    in
    let normals_wireframe =
      flatten (6 * 2 * 4 * 3) (n-1) (m-1)
        (fun _ _ ->
           let top = [| 0.; 1.; 0.|] in
           let bot = [| 0.; -1.; 0.|] in
           let right = [| 1.; 0.; 0.|] in
           let left = [| -1.; 0.; 0.|] in
           let back = [| 0.; 0.; 1.|] in
           let front = [| 0.; 0.; -1.|] in
           Array.concat [
             top;top;top;top;top;top;top;top;
             bot;bot;bot;bot;bot;bot;bot;bot;
             right;right;right;right;right;right;right;right;
             left;left;left;left;left;left;left;left;
             back;back;back;back;back;back;back;back;
             front;front;front;front;front;front;front;front;
           ])
    in
    {
      triangles;
      normals;
      wireframe;
      normals_wireframe;
    }


end

let init_triple_array size f =
  let result = Float32Array.new_float32_array (`Size (3 * size)) in
  let k = ref 0 in
  while !k < size do
    let x,y,z = f !k in
    let pos = 3 * !k in
    incr k;
    Float32Array.set result pos x;
    Float32Array.set result (pos + 1) y;
    Float32Array.set result (pos + 2) z;
  done;
  result
