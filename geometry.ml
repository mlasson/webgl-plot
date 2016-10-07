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

let flatten n m f =
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

let compute_points dim1 dim2 desc =
  let n, m = Array.length dim1, Array.length dim2 in
  flatten n m (fun i j -> desc dim1.(i) dim2.(j))

let compute_normals n m points =
  flatten n m (fun i j ->
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
      if 0 < j && i < n then
        add_one i (j-1) (i+1) j; (* 1 - 2 *)
      if j < m && i < n then
        add_one (i+1) j i (j+1); (* 2 - 3 *)
      if 0 < i && j < m then
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

type indexes =
  [ `Byte of Uint8Array.t
  | `Short of Uint16Array.t
  | `Int of Uint32Array.t]

let indexes_of_array a : indexes =
  let len = Array.length a in
  if len < 256 then
    `Byte (Uint8Array.new_uint8_array (`Data a))
  else if len < 65536 then
    `Short (Uint16Array.new_uint16_array (`Data a))
  else
    `Int (Uint32Array.new_uint32_array (`Data a))

class virtual geometry =
  object(this)
    method virtual points : Float32Array.t
    method virtual normals : Float32Array.t
    method virtual indexes : indexes
    method virtual wireframe : indexes

    method bounding_box = bounding_box (this # points)
  end

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
  indexes_of_array segments

class sphere res =
  let dim1 = uniform_array res 0.0 (2.0 *. pi) in
  let dim2 = uniform_array res 0.0 pi in
  let points = compute_points dim1 dim2
      (fun theta phi ->
         (cos theta *. sin phi, sin theta *. sin phi, cos phi))
  in
  let indexes = triangles_indexes_from_grid res res in
  let lines = lines_indexes_from_grid res res in
  object
    inherit geometry

    method points = points
    method normals = points
    method indexes = indexes
    method wireframe = lines
  end

class copy (geometry : geometry) =
  object
    inherit geometry
    method points = geometry # points
    method normals = geometry # points
    method indexes = geometry # indexes
    method wireframe = geometry # wireframe
  end

class surface xs zs ys =
  let n, m = Array.length xs, Array.length zs in
  let points =
    flatten n m
      (fun i j ->
         (xs.(i), ys.(i*m + j), zs.(j)))
  in
  let normals =
    compute_normals n m points
  in
  let indexes = triangles_indexes_from_grid n m in
  let lines = lines_indexes_from_grid n m in
  object
    inherit geometry

    method points = points
    method normals = normals
    method indexes = indexes
    method wireframe = lines
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

class virtual colored color =
  object(this)
    method virtual points : Float32Array.t
    method virtual normals : Float32Array.t

    val mutable colors = None

    method colors =
      match colors with
      | Some x -> x
      | _ -> assert false

    initializer
      colors <- Some (init_triple_array ((Float32Array.length (this # points)) / 3) (fun k ->
          let pos = 3 * k in
          let p =
            let points = this # points in
            Float32Array.get points pos, Float32Array.get points (pos + 1), Float32Array.get points (pos + 2)
          in
          let n =
            let normals = this # normals in
            Float32Array.get normals pos, Float32Array.get normals (pos + 1), Float32Array.get normals (pos + 2)
          in
          color p n))
  end

