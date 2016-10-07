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

module Array = struct
  include Array
  let min_max a =
    if Array.length a = 0 then None
    else
      let min = ref a.(0) in
      let max = ref !min in
      Array.iter (fun x ->
          if x < !min then min := x;
          if x > !max then max := x) a;
      Some (!min, !max)
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

module Color = struct
  let hsv h s v =
    let c = s *. v in
    let h = h /. 60.0 in
    let x =
      c *. (1.0 -. abs_float (h -. 2.0 *. floor (h /. 2.0) -. 1.0))
    in
    let r,g,b =
      if h < 0. || h > 6. then 0.0, 0.0, 0.0
      else if h < 1.0 then (c,x,0.0)
      else if h < 2.0 then (x,c,0.0)
      else if h < 3.0 then (0.0,c,x)
      else if h < 4.0 then (0.0,x,c)
      else if h < 5.0 then (x,0.0,c)
      else (c,0.0,x)
    in
    let m = v -. c in
    (r +. m, g +. m, b +. m)

  
  let gradient colors =
     let n = Array.length colors in
     fun t ->
       if t <= 0.0 then
         colors.(0)
       else if t >= 1.0 then
         colors.(n-1)
       else
         let t = t *. (float (n - 1)) in
         let k = int_of_float t in
         let t' = t -. (float k) in
         let t = 1.0 -. t' in
         let x, y, z = colors.(k) in
         let x', y', z' = colors.(k+1) in
         x *. t +. x' *. t',
         y *. t +. y' *. t',
         z *. t +. z' *. t'

  let cold_to_hot =
    let colors = [| 
      0.0, 0.0, 1.0;
      0.0, 1.0, 1.0;
      0.0, 1.0, 0.0;
      1.0, 1.0, 0.0;
      1.0, 0.0, 0.0 |] in
    gradient colors

 let white_cold_to_hot =
    let colors = [| 
      1.0, 1.0, 1.0;
      0.0, 0.0, 1.0;
      0.0, 1.0, 1.0;
      0.0, 1.0, 0.0;
      1.0, 1.0, 0.0;
      1.0, 0.0, 0.0 |] in
    gradient colors

end



