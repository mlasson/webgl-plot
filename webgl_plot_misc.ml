open Js_array
open Webgl_plot_math

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


let array_of_float32 a =
  Array.init (Float32Array.length a) (fun k -> Float32Array.get a k)

let float32_array a =
  Float32Array.new_float32_array (`Data a)

module FloatData = struct

  let init size f =
    let result = Float32Array.new_float32_array (`Size size) in
    let k = ref 0 in
    while !k < size do
      incr k;
      Float32Array.set result !k (f !k);
    done;
    result

  let init_array size dim f =
    let result = Float32Array.new_float32_array (`Size (dim * size)) in
    let k = ref 0 in
    while !k < size do
      let a = f !k in
      let pos = dim * !k in
      incr k;
      assert (Array.length a = dim);
      Array.iteri (fun k x ->
        Float32Array.set result (pos + k) x) a;
    done;
    result

  let init3 size f =
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

  let flatten_array_array aa =
    let size =
      let res = ref 0 in
      for k = 0 to Array.length aa - 1 do
        res := !res + Array.length aa.(k);
      done;
      !res
    in
    let res = Float32Array.new_float32_array (`Size size) in
    let pos = ref 0 in
    for i = 0 to Array.length aa - 1 do
      let a = aa.(i) in
      for j = 0 to Array.length a - 1 do
        Float32Array.set res !pos a.(j);
        incr pos;
      done;
    done;
    res

  let flatten_array_array_array aaa =
    let size =
      let res = ref 0 in
      for i = 0 to Array.length aaa - 1 do
        let aa = aaa.(i) in
        for j = 0 to Array.length aa - 1 do
          res := !res + Array.length aa.(j);
        done;
      done;
      !res
    in
    let res = Float32Array.new_float32_array (`Size size) in
    let pos = ref 0 in
    for i = 0 to Array.length aaa - 1 do
      let aa = aaa.(i) in
      for j = 0 to Array.length aa - 1 do
        let a = aa.(j) in
        for k = 0 to Array.length a - 1 do
          Float32Array.set res !pos a.(k);
          incr pos;
        done;
      done;
    done;
    res

  let init2_matrix n m f =
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

  let init3_matrix n m f =
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

  let iteri f a =
    let open Float32Array in
    for k = 0 to length a - 1 do
      f k (get a k)
    done

  let iter f a = iteri (fun _ -> f) a

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

  let min_max a =
    if Float32Array.length a = 0 then None
    else
      let min = ref (Float32Array.get a 0) in
      let max = ref !min in
      iter (fun x ->
          if x < !min then min := x;
          if x > !max then max := x) a;
      Some (!min, !max)
end
