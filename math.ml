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
