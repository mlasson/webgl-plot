let hashtbl_to_list table = 
  let keys = 
    List.sort_uniq compare 
      (Hashtbl.fold (fun k _ acc -> k :: acc) table []) 
  in
  List.rev_map (fun key -> key, Hashtbl.find_all table key) keys

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

  val to_two: two vector -> float * float
  val to_three: three vector -> float * float * float
  val to_four: four vector -> float * float * float * float

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
    val x_rotation: float -> four square_matrix
    val y_rotation: float -> four square_matrix
    val z_rotation: float -> four square_matrix
    val projection: four square_matrix
  end
end = struct
  type 'a vector = float array
  type ('a, 'b) times
  type ('a, 'b) matrix = ('a, 'b) times vector
  type 'a square_matrix = ('a, 'a) matrix

  let of_two (x,y) = [| x; y |]
  let of_three (x,y,z) = [| x; y; z |]
  let of_four (x,y,z,t) = [| x; y; z; t |]

  let to_two = function [| x; y; |] -> (x,y) | _ -> assert false
  let to_three = function [| x; y; z |] -> (x,y,z) | _ -> assert false
  let to_four = function [| x; y; z; t|] -> (x,y,z,t) | _ -> assert false

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

    let projection = [|
      1.; 0.; 0.; 0.;
      0.; 1.; 0.; 0.;
      0.; 0.; -1.; 0.;
      0.; 0.; 0.; 1.;
    |]
  end
end

type vec2 = two Vector.vector
type vec3 = three Vector.vector
type vec4 = four Vector.vector
type mat4 = (four, four) Vector.matrix

module Param = struct

  let iter_range min max steps f =
    let step = (max -. min) /. (float steps) in
    let cur = ref min in
    for _ = 1 to steps do
      let next = !cur +. step in
      f !cur next;
      cur := next;
    done

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

end

module Triangles = struct
  open Vector 

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

  let bounding_box l = 
    let iter f (x,y,z) = 
      f x; f y; f z 
    in
    match l with 
    | ((first, _, _) as hd )::tl ->
      let x,y,z = Vector.to_three first in
      let x_min, x_max = ref x, ref x in
      let y_min, y_max = ref y, ref y in
      let z_min, z_max = ref z, ref z in
      List.iter (fun p -> 
          iter (function next ->
              let x, y, z = Vector.to_three next in
              if x < !x_min then x_min := x;
              if x > !x_max then x_max := x;
              if y < !y_min then y_min := y;
              if y > !y_max then y_max := y;
              if z < !z_min then z_min := z;
              if z > !z_max then z_max := z) p)
        (hd::tl);
      { 
        x_min = !x_min;
        x_max = !x_max;
        y_min = !y_min;
        y_max = !y_max;
        z_min = !z_min;
        z_max = !z_max
      }
    | _ -> failwith "bounding_box: empty list"

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
       if f j then 
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

  let build_boxes triangles = 
    let {x_min; x_max; z_min; z_max; _} : box = bounding_box triangles in
    let boxes = 
      let results = ref [] in
      Param.iter_range x_min x_max 100
        begin fun x x' ->
          Param.iter_range z_min z_max 100
            begin fun z z' ->
              let rect : rect = 
                { 
                  x_min = x; 
                  x_max = x'; 
                  z_min = z; 
                  z_max = z'
                }  
              in
              results := rect :: !results
            end
        end;
      Array.of_list (List.rev !results)
    in 
    let nb_boxes = Array.length boxes in 
    Printf.printf "nb_boxes = %d\n%!" nb_boxes;
    let table = Hashtbl.create 1000 in
    List.iter (fun ((a,b,c) as triangle) -> 
        let x_a, _, z_a = Vector.to_three a in
        let x_b, _, z_b = Vector.to_three b in
        let x_c, _, z_c = Vector.to_three c in  
        let box_of x z = 
          let i = 
            match dicho (fun i -> boxes.(i).x_max >= x) 0 (nb_boxes - 1) with
            | None -> 
              assert (x -. boxes.(nb_boxes - 1).x_max <= 1e-8);
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
              (*
              Printf.printf "%.2f %.2f %d %d\n%!" x z i j;
              Printf.printf "%d: x_min = %.2f, x_max = %.2f, z_min = %.2f, z_max = %.2f\n%!" i (boxes.(i).x_min) (boxes.(i).x_max) (boxes.(i).z_min) (boxes.(i).z_max);
              Printf.printf "%d: x_min = %.2f, x_max = %.2f, z_min = %.2f, z_max = %.2f\n%!" j (boxes.(j).x_min) (boxes.(j).x_max) (boxes.(j).z_min) (boxes.(j).z_max); *)
              assert (z -. boxes.(j).z_max <= 1e-8);
              j
            | Some k -> k
          in
          let box = boxes.(k) in
          Hashtbl.add table box triangle
        in
        box_of x_a z_a;
        box_of x_b z_b;
        box_of x_c z_c
      ) triangles;
    hashtbl_to_list table

  let ray_triangles ?table o e l =
    let d = Vector.sub e o in
    let l =
      match table with 
      | None -> [l]
      | Some table -> 
        let x_d, _, z_d = Vector.to_three d in
        let x_e, _, z_e = Vector.to_three e in
        let d = Vector.of_two (x_d, z_d) in
        let e = Vector.of_two (x_e, z_e) in
        choose (fun (box, triangles) -> 
            if ray_box box d e then
              Some triangles
            else
              None
          ) table
    in
    let size = List.fold_left (fun acc l -> acc + List.length l) 0 l in
    Printf.printf "size = %d, nb_boxes = %d\n%!" size (List.length l);
    let hits =
      List.map (choose (fun (a,b,c) -> triangle_intersection o d a b c)) l
      |> List.flatten
    in
    match
      List.sort (fun (d, _) (d', _) -> compare d d') 
        (List.map (fun p -> Vector.dist e p, p) hits)
    with
    | (_, p) :: _ -> Some p
    | _ -> None

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



