let pi = 3.1415926535897932384626433832795

let translation (x,y,z) = [|
     1.;  0.;  0.;  0.;
     0.;  1.;  0.;  0.;
     0.;  0.;  1.;  0.;
      x;   y;   z;  1.
  |]

let scale (x,y,z) = [|
      x;  0.;  0.;  0.;
     0.;   y;  0.;  0.;
     0.;  0.;   z;  0.;
     0.;  0.;  0.;  1.
|]

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
  | _ -> assert false

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
  | _ -> assert false


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

let make_projection = [|
   1.; 0.; 0.; 0.;
   0.; 1.; 0.; 0.;
   0.; 0.; -1.; 0.;
   0.; 0.; 0.; 1.;
|]

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
          result := [b;a;c] :: [c;a;d] :: !result;
        end
    end;
  List.rev !result

let sphere res =
  parametrize2d
    ~dim1:(0.0, 2.0 *. pi)
    ~dim2:(0.0, pi)
    (res, res)
    (fun theta phi ->
       [cos theta *. sin phi; sin theta *. sin phi; cos phi])

let graph res xmin xmax ymin ymax f =
  parametrize2d
    ~dim1:(xmin, xmax)
    ~dim2:(ymin, ymax)
    (res, res)
    (fun x y -> [x; f x y; y])

module Vector = struct
let add = List.map2 (+.)
let mul = List.map2 ( *. )
let sub = List.map2 (-.)
let sum = List.fold_left (+.) 0.0
let dot e1 e2 = sum (mul e1 e2)
let norm v = sqrt (dot v v)
let dist a b = norm (sub b a)
let normalize v =
  let n = norm v in
  List.map (fun x -> x /. n) v
let scale alpha = List.map (( *.) alpha)

let cross_product v1 v2 = match v1, v2 with
  | [x1; y1; z1], [x2; y2; z2] ->
    let xn = y1 *. z2 -. z1 *. y2 in
    let yn = z1 *. x2 -. x1 *. z2 in
    let zn = x1 *. y2 -. y1 *. x2 in
    [xn;yn;zn]
  | _ -> assert false
end

let normals_of_triangle = function
  | [[x1; y1; z1]; [x2; y2; z2]; [x3; y3; z3]] ->
    let xn = (y2 -. y1) *. (z3 -. z1) -. (z2 -. z1) *. (y3 -. y1) in
    let yn = (z2 -. z1) *. (x3 -. x1) -. (x2 -. x1) *. (z3 -. z1) in
    let zn = (x2 -. x1) *. (y3 -. y1) -. (y2 -. y1) *. (x3 -. x1) in
    let n = sqrt (xn *. xn +. yn *. yn +. zn *. zn) in
    let d = [xn /. n; yn /. n; zn /. n] in
    [d;d;d]
  | _ -> assert false

let rectangles_of_triangles l =
  let rec aux acc = function
    | t1::t2::tl -> aux ([t1;t2] :: acc) tl
    | [] -> List.rev acc
    | _ -> assert false
  in
  aux [] l

let hsv h s v =
  let c = s *. v in
  let h = h /. 60.0 in
  let x = c *. (1.0 -. abs_float (h -. 2.0 *. floor (h /. 2.0) -. 1.0)) in
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
  [r +. m; g +. m; b +. m]


let epsilon = 0.000001

(* https://en.wikipedia.org/wiki/M%C3%B6ller%E2%80%93Trumbore_intersection_algorithm *)
let triangle_intersection o d v1 v2 v3 =
  let open Vector in
  let e1 = sub v2 v1 in
  let e2 = sub v3 v1 in
  let p = cross_product d e2 in
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
     let q = cross_product t e1 in
     let v = dot d q *. inv_det in
     if v < 0. || u +. v > 1. then
       None
     else
       let t = dot e2 q *. inv_det in
       if t > epsilon then
         Some (add o (scale t d))
       else
         None

let choose f l =
  let rec aux acc = function [] -> List.rev acc
    | hd :: tl ->
      begin match f hd with
        | Some x -> aux (x :: acc) tl
        | None -> aux acc tl
      end
  in aux [] l

let ray_triangles o e l =
  let d = Vector.sub e o in
  let hits =
    choose (function [a;b;c] -> triangle_intersection o d a b c | _ -> assert false) l
  in
  match List.sort compare (List.map (fun p -> Vector.dist e p, p) hits) with
  | (_, p) :: _ -> Some p
  | _ -> None
