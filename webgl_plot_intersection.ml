(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

open Js_array
open Webgl_plot_misc
module Math = Webgl_plot_math
open Math

let debug = false
let epsilon = 1e-9

let iter_triangles indexes f =
  let size = Index.length indexes / 3 in
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

let vec3_of_array a =
  match Vector.of_array a with
  | `Three v -> v
  | _ -> failwith "vec3_of_array"

let get3 buffer k =
  let tmp = Array.create_float 3 in
  get_generic tmp buffer k;
  vec3_of_array tmp


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

let hashtbl_to_list table =
  let keys =
    List.sort_uniq compare
      (Hashtbl.fold (fun k _ acc -> k :: acc) table [])
  in
  List.map (fun key -> key, Hashtbl.find_all table key) keys

let build_ray_table points triangles =
  let nb_triangles = Index.length triangles / 3 in
  let size = max (int_of_float (sqrt (float nb_triangles) /. 10.0)) 1 in
  let boxes = partition points size in
  let nb_boxes = Array.length boxes in
  let table = Hashtbl.create nb_boxes in
  iter_triangles triangles (fun ((a,b,c) as triangle) ->
      let x_a, _, z_a = Vector.to_three (get3 points a) in
      let x_b, _, z_b = Vector.to_three (get3 points b) in
      let x_c, _, z_c = Vector.to_three (get3 points c) in
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
    );
  hashtbl_to_list table

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
        let a = get3 points a in
        let b = get3 points b in
        let c = get3 points c in
        triangle_intersection o d a b c)) l
    |> List.flatten
  in
  match
    List.sort (fun (d, _) (d', _) -> compare d d')
      (List.map (fun p -> Vector.dist e p, p) hits)
  with
  | (_, p) :: _ -> Some p
  | _ -> None
