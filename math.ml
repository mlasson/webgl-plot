open Js_bindings

let debug = false

module List = struct
  include List

  let choose f l =
    let rec aux acc = function
      | [] -> List.rev acc
      | hd :: tl ->
        begin match f hd with
          | Some hd -> aux (hd :: acc) tl
          | None -> aux acc tl
        end
    in
    aux [] l

  let flatten l =
    let rec aux acc =
      function
      | [] -> List.rev acc
      | hd :: tl -> aux (List.rev_append hd acc) tl
    in aux [] l

  let map f l = List.rev_map f l |> List.rev
end

type four
type three
type two

let pi = 3.1415926535897932384626433832795
let sq x = x *. x

module Vector : sig
  type 'a vector = private float array
  type ('a, 'b) times
  type ('a, 'b) matrix = ('a, 'b) times vector
  type 'a square_matrix = ('a, 'a) matrix

  val of_two: float * float -> two vector
  val of_three: float * float * float -> three vector
  val of_four: float * float * float * float -> four vector
  val of_array: float array -> [ `Three of three vector |
                                 `Two of two vector |
                                 `Four of four vector ]

  val to_two: two vector -> float * float
  val to_three: three vector -> float * float * float
  val to_four: four vector -> float * float * float * float
  val to_array: 'a vector -> float array
  val to_string: 'a vector -> string

  val add: 'a vector -> 'a vector -> 'a vector
  val sub: 'a vector -> 'a vector -> 'a vector
  val mul: 'a vector -> 'a vector -> 'a vector

  val sum: 'a vector -> float

  val dot: 'a vector -> 'a vector -> float
  val norm: 'a vector -> float
  val dist: 'a vector -> 'b vector -> float

  val scale: float -> 'a vector -> 'a vector
  val normalize: 'a vector -> 'a vector

  val four_to_three: four vector -> three vector
  val cross: three vector -> three vector -> three vector

  val flatten: 'a vector list -> float array

  val multiply: (four, four) matrix -> (four, four) matrix -> (four, four) matrix
  val multiply_vector: (four, four) matrix -> four vector -> four vector

  module Const : sig
    val scale: three vector -> four square_matrix
    val translation: three vector -> four square_matrix
    val scale_translation: three vector -> three vector -> four square_matrix
    val x_rotation: float -> four square_matrix
    val y_rotation: float -> four square_matrix
    val z_rotation: float -> four square_matrix
    val identity: four square_matrix
    val flip: four square_matrix
    val projection: fov:float -> aspect:float -> near:float -> far:float -> four square_matrix
    val inverse_projection: fov:float -> aspect:float -> near:float -> far:float -> four square_matrix
  end
end = struct
  type 'a vector = float array
  type ('a, 'b) times
  type ('a, 'b) matrix = ('a, 'b) times vector
  type 'a square_matrix = ('a, 'a) matrix

  let of_two (x,y) = [| x; y |]
  let of_three (x,y,z) = [| x; y; z |]
  let of_four (x,y,z,t) = [| x; y; z; t |]
  let of_array a =
    match Array.length a with
    | 2 -> `Two a
    | 3 -> `Three a
    | 4 -> `Four a
    | _ -> failwith "Vector.of_array: unsupported length"

  let to_two = function [| x; y; |] -> (x,y) | _ -> assert false
  let to_three = function [| x; y; z |] -> (x,y,z) | _ -> assert false
  let to_four = function [| x; y; z; t|] -> (x,y,z,t) | _ -> assert false
  let to_array = fun x -> x
  let to_string = fun x -> String.concat "," (List.map string_of_float (Array.to_list x))

  let add = Array.map2 (+.)
  let sub = Array.map2 (-.)
  let mul = Array.map2 ( *. )
  let sum = Array.fold_left (+.) 0.0
  let dot u v =
    let n = Array.length u in
    assert (Array.length v = n);
    let res = ref 0.0 in
    for i = 0 to n - 1 do
      res := !res +. u.(i) *. v.(i)
    done;
    !res
  let norm v = sqrt (dot v v)
  let scale alpha = Array.map (( *. ) alpha)
  let normalize v = scale (1.0 /. norm v) v

  let four_to_three = function
    | [| x;y;z;t |] ->
      [| x /. t; y /. t; z /. t |]
    | _ -> assert false

  let dist u v =
    let n = Array.length u in
    assert (Array.length v = n);
    let res = ref 0.0 in
    for i = 0 to n - 1 do
      res := !res +. sq (u.(i) -. v.(i))
    done;
    sqrt !res

  let cross v1 v2 =
    let x1, y1, z1 = to_three v1 in
    let x2, y2, z2 = to_three v2 in
    let xn = y1 *. z2 -. z1 *. y2 in
    let yn = z1 *. x2 -. x1 *. z2 in
    let zn = x1 *. y2 -. y1 *. x2 in
    [|xn;yn;zn|]

  let flatten = Array.concat

  let multiply a b =
    match a, b with
    | [|a11;a21;a31;a41;
        a12;a22;a32;a42;
        a13;a23;a33;a43;
        a14;a24;a34;a44|],
      [|b11;b21;b31;b41;
        b12;b22;b32;b42;
        b13;b23;b33;b43;
        b14;b24;b34;b44|] ->
      [|
        a11*.b11 +. a12*.b21 +. a13*.b31 +. a14*.b41;
        a21*.b11 +. a22*.b21 +. a23*.b31 +. a24*.b41;
        a31*.b11 +. a32*.b21 +. a33*.b31 +. a34*.b41;
        a41*.b11 +. a42*.b21 +. a43*.b31 +. a44*.b41;

        a11*.b12 +. a12*.b22 +. a13*.b32 +. a14*.b42;
        a21*.b12 +. a22*.b22 +. a23*.b32 +. a24*.b42;
        a31*.b12 +. a32*.b22 +. a33*.b32 +. a34*.b42;
        a41*.b12 +. a42*.b22 +. a43*.b32 +. a44*.b42;

        a11*.b13 +. a12*.b23 +. a13*.b33 +. a14*.b43;
        a21*.b13 +. a22*.b23 +. a23*.b33 +. a24*.b43;
        a31*.b13 +. a32*.b23 +. a33*.b33 +. a34*.b43;
        a41*.b13 +. a42*.b23 +. a43*.b33 +. a44*.b43;

        a11*.b14 +. a12*.b24 +. a13*.b34 +. a14*.b44;
        a21*.b14 +. a22*.b24 +. a23*.b34 +. a24*.b44;
        a31*.b14 +. a32*.b24 +. a33*.b34 +. a34*.b44;
        a41*.b14 +. a42*.b24 +. a43*.b34 +. a44*.b44|]
    | _ -> failwith "multiply: dim not supported"

  let multiply_vector a b =
    match a, b with
    | [|a11;a21;a31;a41;
        a12;a22;a32;a42;
        a13;a23;a33;a43;
        a14;a24;a34;a44|],
      [|b1;b2;b3;b4|] ->
      [|
        a11*.b1 +. a12*.b2 +. a13*.b3 +. a14*.b4;
        a21*.b1 +. a22*.b2 +. a23*.b3 +. a24*.b4;
        a31*.b1 +. a32*.b2 +. a33*.b3 +. a34*.b4;
        a41*.b1 +. a42*.b2 +. a43*.b3 +. a44*.b4; |]
    | _ -> failwith "multiply: dim not supported"

  module Const = struct

    let translation v =
      let (x,y,z) = to_three v in
      [|
        1.;  0.;  0.;  0.;
        0.;  1.;  0.;  0.;
        0.;  0.;  1.;  0.;
        x;   y;   z;  1.
      |]

    let scale v =
      let (x,y,z) = to_three v in
      [|
        x;  0.;  0.;  0.;
        0.;   y;  0.;  0.;
        0.;  0.;   z;  0.;
        0.;  0.;  0.;  1.
      |]

    let scale_translation a v =
      let (a,b,c) = to_three a in
      let (x,y,z) = to_three v in
      [|
         a;  0.;  0.;  0.;
        0.;   b;  0.;  0.;
        0.;  0.;   c;  0.;
         x;   y;   z;  1.
      |]

    let x_rotation rad =
      let c = cos rad in
      let s = sin rad in [|
        1.;  0.;  0.;  0.;
        0.;  c;  s;  0.;
        0.;  -. s; c;  0.;
        0.;  0.;  0.;  1.
      |]

    let y_rotation rad =
      let c = cos rad in
      let s = sin rad in [|
        c;  0.;  -.s;  0.;
        0.;  1.; 0.;  0.;
        s;  0.; c;  0.;
        0.;  0.;  0.;  1.
      |]

    let z_rotation rad =
      let c = cos rad in
      let s = sin rad in [|
        c;  s;  0.;  0.;
        -.s;  c;  0.;  0.;
        0.;  0.;  1.;  0.;
        0.;  0.;  0.;  1.
      |]

    let identity =  [|
        1.; 0.;  0.;  0.;
        0.; 1.;  0.;  0.;
        0.; 0.;  1.;  0.;
        0.; 0.;  0.;  1.
      |]

    let flip =  [|
        1.; 0.;  0.;  0.;
        0.; 1.;  0.;  0.;
        0.; 0.;  -1.;  0.;
        0.; 0.;  0.;  1.
      |]



    let projection ~fov ~aspect ~near ~far =
      let f = tan ((pi -. fov) /. 2.0 ) in
      let r = 1.0 /. (near -. far) in
        [|
          f /. aspect; 0.; 0.; 0.;
          0.; f ; 0.; 0.;
          0.; 0.; r *. (near +. far); -1.;
          0.; 0.; near *. far *. r *. 2.0; 0.;
        |]

    let inverse_projection ~fov ~aspect ~near ~far =
      let f = tan ((pi -. fov) /. 2.0 ) in
      let r = 1.0 /. (near -. far) in
        [|
          aspect /. f; 0.; 0.; 0.;
          0.; 1.0 /. f ; 0.; 0.;
          0.; 0.; 0.; 0.5 /. (near *. far *. r);
          0.; 0.; -1.; (near +. far) /. (2.0 *. near *. far) ;
        |]

  end
end

type vec2 = two Vector.vector
type vec3 = three Vector.vector
type vec4 = four Vector.vector
type mat4 = (four, four) Vector.matrix

module Buffer = struct
  open Js_bindings


  let iter_generic dim float_array f =
    let tmp = Array.create_float dim in
    Float32Array.iteri (fun k x ->
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

  let number_of_triangles indexes = (Uint16Array.length indexes) / 3

  let iter_triangles ?(chunk_size = 1000) indexes f =
    let size = number_of_triangles indexes in
    Asynchronous_computations.range_chunks chunk_size (fun k ->
        f (Uint16Array.get indexes (3 * k),
           Uint16Array.get indexes (3 * k + 1),
           Uint16Array.get indexes (3 * k + 2))
      ) 0 (size - 1)


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

module Param = struct

  let grid_of_fun
      ?(dim1 = (0.0, 1.0))
      ?(dim2 = (0.0, 1.0)) (res1, res2) f =
    print_endline "grid_of_fun";
    let open Asynchronous_computations in
    let min1 = fst dim1 in
    let max1 = snd dim1 in
    let scale1 = (max1 -. min1) /. (float (res1 - 1)) in
    let min2 = fst dim2 in
    let max2 = snd dim2 in
    let scale2 = (max2 -. min2) /. (float (res2 - 1)) in

    let result = Array.make res1 [||] in
    range (fun i ->
        let x = min1 +. scale1 *. (float i) in
        result.(i) <- Array.init res2 (fun j ->
            let y = min2 +. scale2 *. (float j) in
            f x y
          ); return ()) 0 (res1 - 1) >>= fun () -> return result

  let triangles_of_grid grid =
    let open Asynchronous_computations in
    let result = ref [] in
    range (fun i ->
        for j = 0 to (Array.length grid.(i)) - 2 do
          let a = grid.(i).(j) in
          let b = grid.(i+1).(j) in
          let c = grid.(i+1).(j+1) in
          let d = grid.(i).(j+1) in
          result := (b,a,c) :: (c,a,d) :: !result;
        done;
        return ()
      ) 0 ((Array.length grid) - 2) >>= fun () ->
        return !result

  let lines_of_grid grid =
    let open Asynchronous_computations in
    let result = ref [] in
    range (fun i ->
        for j = 0 to (Array.length grid.(i)) - 2 do
          let a = grid.(i).(j) in
          let b = grid.(i+1).(j) in
          let c = grid.(i+1).(j+1) in
          let d = grid.(i).(j+1) in
          result := (a,b) :: (b,c) :: (c,d) :: (d,a):: !result;
        done;
        return ()
      ) 0 ((Array.length grid) - 2) >>= fun () ->
        return !result

  let iter_range min max steps f =
    let step = (max -. min) /. (float steps) in
    let cur = ref min in
    for _ = 1 to steps do
      let next = !cur +. step in
      f !cur next;
      cur := next;
    done

  let iter_range_computation min max steps f =
    let step = (max -. min) /. (float steps) in
    let cur = ref min in
    Asynchronous_computations.range (fun _ ->
      let next = !cur +. step in
      f !cur next;
      cur := next;
      Asynchronous_computations.delay ()) 1 steps

  let parametrize2d ?(dim1 = (0.0, 1.0))
      ?(dim2 = (0.0, 1.0)) (res1, res2) f =
    let result = ref [] in
    iter_range (fst dim1) (snd dim1) res1
      begin fun t1 t1' ->
        iter_range (fst dim2) (snd dim2) res2
          begin fun t2 t2' ->
            let a, b, c, d = f t1 t2, f t1' t2, f t1' t2', f t1 t2' in
            result := (b,a,c) :: (c,a,d) :: !result;
          end
      end;
    List.rev !result

  let parametrize2d_computation ?(context = Asynchronous_computations.console_context)
      ?(dim1 = (0.0, 1.0))
      ?(dim2 = (0.0, 1.0)) (res1, res2) f =
    let result = ref [] in
    let cpt = ref 0 in
    context # status "Computing the graph of the function ...";
    context # push;
    Asynchronous_computations.(bind
      (iter_range_computation (fst dim1) (snd dim1) res1
         begin fun t1 t1' ->
           context # progress ((float !cpt) /. (float res1));
           incr cpt;
           iter_range (fst dim2) (snd dim2) res2
             begin fun t2 t2' ->
               let a, b, c, d = f t1 t2, f t1' t2, f t1' t2', f t1 t2' in
               result := (b,a,c) :: (c,a,d) :: !result;
             end
         end)
      (fun () -> context # pop; return (List.rev !result)))


  let sphere res =
    parametrize2d
      ~dim1:(0.0, 2.0 *. pi)
      ~dim2:(0.0, pi)
      (res, res)
      (fun theta phi ->
         Vector.of_three
           (cos theta *. sin phi, sin theta *. sin phi, cos phi))

  let graph res xmin xmax ymin ymax f =
    parametrize2d
      ~dim1:(xmin, xmax)
      ~dim2:(ymin, ymax)
      (res, res)
      (fun x y -> Vector.of_three (x, f x y, y))

  let graph_computation ?context res xmin xmax ymin ymax f =
    parametrize2d_computation ?context
      ~dim1:(xmin, xmax)
      ~dim2:(ymin, ymax)
      (res, res)
      (fun x y -> Vector.of_three (x, f x y, y))
end

module Triangles = struct
  open Vector

  type t = vec3 * vec3 * vec3

  let normal_of_triangle a b c =
    let x1, y1, z1 = Vector.to_three a in
    let x2, y2, z2 = Vector.to_three b in
    let x3, y3, z3 = Vector.to_three c in
    let xn = (y2 -. y1) *. (z3 -. z1) -. (z2 -. z1) *. (y3 -. y1) in
    let yn = (z2 -. z1) *. (x3 -. x1) -. (x2 -. x1) *. (z3 -. z1) in
    let zn = (x2 -. x1) *. (y3 -. y1) -. (y2 -. y1) *. (x3 -. x1) in
    let n = sqrt (xn *. xn +. yn *. yn +. zn *. zn) in
    (of_three (xn /. n, yn /. n, zn /. n))

  let epsilon = 0.0000001

  (* https://en.wikipedia.org/wiki/M%C3%B6ller%E2%80%93Trumbore_intersection_algorithm *)
  let triangle_intersection o d v1 v2 v3 =
    let open Vector in
    let e1 = sub v2 v1 in
    let e2 = sub v3 v1 in
    let p = cross d e2 in
    let det = dot e1 p in
    if det > -. epsilon && det < epsilon then
      None
    else
      let inv_det = 1.0 /. det in
      let t = sub o v1 in
      let u = dot t p *. inv_det in
      if u < 0. || u > 1. then
        None
      else
        let q = cross t e1 in
        let v = dot d q *. inv_det in
        if v < 0. || u +. v > 1. then
          None
        else
          let t = dot e2 q *. inv_det in
          if t > epsilon then
            Some (add o (scale t d))
          else
            None

  type box = {
    x_min : float;
    x_max : float;
    y_min : float;
    y_max : float;
    z_min : float;
    z_max : float;
  }

  let bounding_box points =
    if Float32Array.length points < 3 then failwith "bounding_box: not enough points" else
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
    Float32Array.iteri (fun k v ->
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

  type rect = {
    x_min : float;
    z_min : float;
    x_max : float;
    z_max : float;
  }

  let ray_box ({x_min; x_max; z_min; z_max} : rect) (a : vec2) (b: vec2) =
    let (x_a,z_a) = Vector.to_two a in
    let (x_b,z_b) = Vector.to_two b in
    if abs_float x_a < epsilon then
      x_min < x_b && x_b < x_max
    else if abs_float z_a < epsilon then
      z_min < z_b && z_b < z_max
    else begin
      let l = (x_min -. x_b) /. x_a in
      let l' = (z_min -. z_b) /. z_a in

      let r = (x_max -. x_b) /. x_a in
      let r' = (z_max -. z_b) /. z_a in

      if x_a > 0.0 then
        if z_a > 0.0 then
          max l l' <= min r r'
        else
          max l r' <= min r l'
      else
        if z_a > 0.0 then
          max r l' <= min l r'
        else
          max r r' <= min l l'
    end

  let rec dicho f i j =
    if i >= j then
       if i = j && f j then
         Some j
       else
         None
    else
      let p = (i + j) / 2 in
      if f p then
        dicho f i p
      else
        dicho f (p + 1) j

  let rec forward f i j =
    if f (i + j) then
      if j = 1 then
        i + j
      else
        forward f (i + (j / 2)) 1
    else
      forward f i (2 * j)

  type ray_table = (rect * (int * int * int) list) list

  let partition points size =
    let len = (Float32Array.length points) / 3 in
    let xs = Array.create_float len in
    let zs = Array.create_float len in
    for k = 0 to len - 1 do
      let x = Float32Array.get points (3 * k) in
      let z = Float32Array.get points (3 * k + 2) in
       xs.(k) <- x;
       zs.(k) <- z;
    done;
    Array.sort compare xs;
    Array.sort compare zs;
    let bag = len / size in
    let results = ref [] in
    for i = 0 to size - 1 do
      for j = 0 to size - 1 do
        let get_bounds k a =
          if k = 0 then a.(0)
          else if k = size then a.(len - 1)
          else let pos = k * bag in
          0.5 *. (a.(pos + 1) +. a.(pos))
        in
        let x_min = get_bounds i xs in
        let x_max = get_bounds (i + 1) xs in
        let z_min = get_bounds j zs in
        let z_max = get_bounds (j + 1) zs in
        let rect : rect =
          {
            x_min;
            x_max;
            z_min;
            z_max
          }
        in
        results := rect :: !results
      done
    done;
    Array.of_list (List.rev !results)


  let build_ray_table context points triangles =
    let nb_triangles = Buffer.number_of_triangles triangles in
    let size = max (int_of_float (sqrt (float nb_triangles) /. 10.0)) 1 in
    let boxes = partition points size in
    let nb_boxes = Array.length boxes in
    let table = Hashtbl.create nb_boxes in
    let chunks = 1000 in
    let cpt = ref 0 in
    context # status "Computing ray casting table ...";
    context # push;
    let open Asynchronous_computations in
    let delay () =
      cpt := !cpt + chunks;
      context # progress ((float !cpt) /. (float nb_triangles));
      delay ()
    in
    Buffer.iter_triangles triangles (fun ((a,b,c) as triangle) ->
        let x_a, _, z_a = Vector.to_three (Buffer.get3 points a) in
        let x_b, _, z_b = Vector.to_three (Buffer.get3 points b) in
        let x_c, _, z_c = Vector.to_three (Buffer.get3 points c) in
        let x_min = min x_a (min x_b x_c) in
        let x_max = max x_a (max x_b x_c) in
        let z_min = min z_a (min z_b z_c) in
        let z_max = max z_a (max z_b z_c) in
        let box_of x z =
          let i =
            match dicho (fun i -> boxes.(i).x_max >= x) 0 (nb_boxes - 1) with
            | None ->
              nb_boxes - 1
            | Some i -> i
          in
          let x_bound = boxes.(i).x_max in
          let j =
            (forward (fun k ->
                 k >= nb_boxes || boxes.(k).x_max > x_bound) i 1) - 1
          in
          let k =
            match dicho (fun i -> boxes.(i).z_max >= z) i j with
            | None ->
              j
            | Some k -> k
          in
          k
        in
        let bl = box_of x_min z_min in
        let br = box_of x_min z_max in
        let tl = box_of x_max z_min in
        let n = (br - bl) in
        let m = (tl - bl) / size in
        for i = 0 to n do
          for j = 0 to m do
            Hashtbl.add table boxes.(bl + j * size + i) triangle;
          done;
        done


      ) >>= fun () -> context # pop; delay () >>= fun () ->
         context # status "Almost done ...";
         Asynchronous_computations.hashtbl_to_list context table

  let ray_triangles points table o e =
    let d = Vector.sub e o in
    let l =
      let x_d, _, z_d = Vector.to_three d in
      let x_e, _, z_e = Vector.to_three e in
      let d = Vector.of_two (x_d, z_d) in
      let e = Vector.of_two (x_e, z_e) in
      List.choose (fun (box, triangles) ->
          if ray_box box d e then begin
            if debug then begin
               let {x_min; z_min; x_max; z_max} = (box : rect) in
               Printf.printf "box(%.2f,%.2f,%.2f,%.2f)\n%!" x_min x_max z_min z_max
            end;
            Some triangles
          end else
            None
        ) table
    in
    if debug then
       Printf.printf "nb_boxes : %d / %d\n%!" (List.length l) (List.length table);
    let hits =
      List.map (List.choose (fun (a,b,c) ->
        let a = Buffer.get3 points a in
        let b = Buffer.get3 points b in
        let c = Buffer.get3 points c in
        triangle_intersection o d a b c)) l
      |> List.flatten
    in
    match
      List.sort (fun (d, _) (d', _) -> compare d d')
        (List.map (fun p -> Vector.dist e p, p) hits)
    with
    | (_, p) :: _ -> Some p
    | _ -> None

  let triangles_indexes_from_grid dim1 dim2 =
    let result = Uint16Array.new_uint16_array (`Size (dim1 * dim2 * 2 * 3)) in
    let k = ref 0 in
    for i = 0 to dim1 - 2 do
      for j = 0 to dim2 - 2 do
        Uint16Array.set result !k (i * dim2 + j); incr k;
        Uint16Array.set result !k ((i + 1) * dim2 + j); incr k;
        Uint16Array.set result !k (i * dim2 + j + 1); incr k;
        Uint16Array.set result !k ((i + 1) * dim2 + j); incr k;
        Uint16Array.set result !k ((i + 1) * dim2 + j + 1); incr k;
        Uint16Array.set result !k (i * dim2 + j + 1); incr k;
      done
    done;
    result

  let normal_grid grid =
    let open Asynchronous_computations in
    let i_max = Array.length grid - 1 in
    let result = Array.make (i_max + 1) [||] in
    print_endline "normal_grid";
    range (fun i ->
      let j_max = Array.length grid.(i) - 1 in
      result.(i) <- Array.init (j_max + 1)
        begin fun j ->
          let res = ref (Vector.of_three (0., 0., 0.)) in
          let cpt = ref 0.0 in
          let o = grid.(i).(j) in
          let add_one a b =
            res := Vector.add !res (normal_of_triangle o a b);
            cpt := !cpt +. 1.0;
          in
          (*     1
           *    /|\
           *   4-O-2
           *    \|/
           *     3
           **)
          if 0 < j && i < i_max then
            add_one grid.(i).(j-1) grid.(i+1).(j); (* 1 - 2 *)
          if j < j_max && i < i_max then
            add_one grid.(i+1).(j) grid.(i).(j+1); (* 2 - 3 *)
          if 0 < i && j < j_max then
            add_one grid.(i).(j+1) grid.(i-1).(j); (* 3 - 4 *)
          if 0 < i && 0 < j then
            add_one grid.(i-1).(j) grid.(i).(j-1); (* 4 - 1 *)

          Vector.scale (1.0 /. !cpt) !res
        end;
      return ()) 0 i_max >>= fun () ->
    print_endline "done";
    return result

end

module Color = struct
  let hsv h s v =
    let c = s *. v in
    let h = h /. 60.0 in
    let x =
      c *. (1.0 -. abs_float (h -. 2.0 *. floor (h /. 2.0) -. 1.0))
    in
    let r,g,b =
      if h < 0. || h >= 60. then 0.0, 0.0, 0.0
      else if h < 1.0 then (c,x,0.0)
      else if h < 2.0 then (x,c,0.0)
      else if h < 3.0 then (0.0,c,x)
      else if h < 4.0 then (0.0,x,c)
      else if h < 5.0 then (x,0.0,c)
      else (c,0.0,x)
    in
    let m = v -. c in
    Vector.of_three (r +. m, g +. m, b +. m)
end



