open Js_array

open Webgl_plot_math
open Webgl_plot_geometry
open Webgl_plot_drawable

module Math = Webgl_plot_math
module Shaders = Webgl_plot_shaders
module Textures = Webgl_plot_textures
module Export = Webgl_plot_export

let delay f =
  let open Js_windows in
  Window.set_timeout window f 0 |> ignore

let build_cube gl texture_shader {x_min; x_max; y_min; y_max; z_min; z_max} =
  let triangles, texcoords =
    let tl, tr, bl, br =
      [0.; 0.], [1.0; 0.],
      [0.; 1.0], [1.0; 1.0]
    in
    let a = [x_min;y_min;z_min] in
    let b = [x_min;y_max;z_min] in
    let c = [x_max;y_max;z_min] in (*   F---G *)
    let d = [x_max;y_min;z_min] in (*  /|  /|  *)
    let e = [x_min;y_min;z_max] in (* B---C |  *)
    let f = [x_min;y_max;z_max] in (* | E-|-H  *)
    let g = [x_max;y_max;z_max] in (* |/  |/   *)
    let h = [x_max;y_min;z_max] in (* A---D    *)

    let front =
      [[a,br;b,tr;c,tl];[a,br;c,tl;d,bl]]
    in
    let left =
      [[b,tl;a,bl;f,tr];[f,tr;a,bl;e,br]]
    in
    let bottom =
      [[a,bl;d,br;h,tr];[a,bl;h,tr;e,tl]]
    in
    let top =
      [[c,tr;b,tl;g,br];[g,br;b,tl;f,bl]]
    in
    let right =
      [[d,br;c,tr;h,bl];[h,bl;c,tr;g,tl]]
    in
    let back =
      [[f,tl;e,bl;g,tr];[g,tr;e,bl;h,br]]
    in
    [front;
     left;
     bottom;
     top;
     right;
     back; ]
    |> List.flatten
    |> List.map List.split
    |> List.split
  in
  let flatten_array l =
    Float32Array.new_float32_array
      (`Data (Array.of_list (List.flatten (List.flatten l))))
  in
  let cube = new Shaders.Texture.drawable gl texture_shader in
  cube # set_triangles (flatten_array triangles);
  cube # set_texcoords (flatten_array texcoords);
  delay (fun () -> cube # set_texture (Textures.create_face_texture ()));
  cube

let coerce min max l =
  let l = Array.of_list (List.filter (fun ({value; _} : Export.tick) -> min <= value && value <= max) l) in
  Array.sort (fun ({value = x; _} : Export.tick) {value = y; _} -> compare x y) l;
  let values = Array.map (fun {Export.value; _} -> ((value -. min) /. (max -. min))) l in
  let texts = Array.map (fun ({label; _} : Export.tick) -> label) l in
  {Textures.values; texts}


let draw_axis gl texture_shader
    {x_min; x_max; y_min; y_max; z_max; z_min} (x_scale, y_scale, z_scale)
    x_axis_label x_axis_ticks y_axis_label y_axis_ticks z_axis_label z_axis_ticks =

  let x_range = x_max -. x_min in
  let y_range = y_max -. y_min in
  let z_range = z_max -. z_min in

  let x_ratio = x_range /. x_scale in
  let y_ratio = y_range /. y_scale in
  let z_ratio = z_range /. z_scale in

  let face_textures = [|
    lazy (Textures.create_ticks_texture x_ratio x_axis_label
      (coerce x_min x_max x_axis_ticks));
    lazy (Textures.create_ticks_texture y_ratio y_axis_label
            (coerce y_min y_max y_axis_ticks));
    lazy (Textures.create_ticks_texture z_ratio z_axis_label
            (coerce z_min z_max z_axis_ticks));
  |] in
  let memo_table = Hashtbl.create 20 in
  let draw_face (front_facing, flip, a, u, v, texture_id) =
    (*
       C---D
     v |   |
       A---B
         u
     *)
    let a, u, v = Vector.of_three a, Vector.of_three u, Vector.of_three v in
    let bl, br, tl, tr =
      (* These are texture positions: *)
      let x_min =
        match front_facing, flip with
        | true, true -> 0.0
        | false, false -> 0.5
        | false, true -> 0.25
        | true, false -> 0.75
      in
      let x_max = x_min +. 0.25 in
      let x_min, x_max =
        if front_facing <> flip then x_min, x_max else x_max, x_min
      in
      let y_min = if flip then 1.0 else 0.0 in
      let y_max = 1.0 -. y_min in
      (x_min, y_max), (x_max, y_max), (x_min, y_min), (x_max, y_min)
    in
    let open Vector in
    let aa = a, Vector.of_two bl in
    let bb = add a u, Vector.of_two br in
    let (c, _) as cc = add a v, Vector.of_two tl  in
    let dd = add c u, Vector.of_two tr in
    let triangles, texcoords =
     if front_facing then
       List.split [aa;bb;cc;dd;cc;bb]
     else
       List.split [aa;cc;bb;cc;dd;bb]
    in
    let triangles = Float32Array.new_float32_array (`Data (flatten triangles)) in
    let texcoords = Float32Array.new_float32_array (`Data (flatten texcoords)) in
    let face = new Shaders.Texture.drawable gl texture_shader in
    face # set_triangles triangles;
    face # set_texcoords texcoords;
    delay (fun () -> face # set_texture (Lazy.force face_textures.(texture_id)));
    face
  in
  let build_face arg =
    match Hashtbl.find memo_table arg with
    | exception Not_found ->
       let res = draw_face arg in
       Hashtbl.add memo_table arg res;
       res
    | res -> res
  in
  let draw_face front_facing flip a u v texture_id =
    (build_face (front_facing, flip, a, u, v, texture_id) ) # draw
  in

  let texture_padding = 1.0 /. 0.9 in
  let y_min' = y_min -. 0.5 *. y_range *.  (texture_padding -. 1.) in
  let x_min' = x_min -. 0.5 *. x_range *.  (texture_padding -. 1.) in
  let z_min' = z_min -. 0.5 *. z_range *.  (texture_padding -. 1.) in

  let draw_y_axis front_facing = function
    | `Back, `Left ->
      draw_face front_facing false
        (x_min, y_min', z_max)
        (-. 0.25 *. x_scale, 0.0, 0.0)
        (0.0, y_range *. texture_padding, 0.0)
        1
    | `Back, `Right ->
      draw_face front_facing false
        (x_max, y_min', z_max)
        (0.25 *. x_scale, 0.0, 0.0)
        (0.0, y_range *. texture_padding, 0.0)
        1
    | `Front, `Left ->
      draw_face front_facing false
        (x_max, y_min', z_min)
        (0.25 *. x_scale, 0.0, 0.0)
        (0.0, y_range *. texture_padding, 0.0)
        1
    | `Front, `Right ->
      draw_face front_facing false
        (x_min, y_min', z_min)
        (-. 0.25 *. x_scale, 0.0, 0.0)
        (0.0, y_range *. texture_padding, 0.0)
        1
    | `Left, `Left ->
      draw_face front_facing false
        (x_min, y_min', z_min)
        (0.0, 0.0, -. 0.25 *. z_scale)
        (0.0, y_range *. texture_padding, 0.0)
        1
    | `Left, `Right ->
      draw_face front_facing false
        (x_min, y_min', z_max)
        (0.0, 0.0, 0.25 *. z_scale)
        (0.0, y_range *. texture_padding, 0.0)
        1
    | `Right, `Left ->
      draw_face front_facing false
        (x_max, y_min', z_max)
        (0.0, 0.0, 0.25 *. z_scale)
        (0.0, y_range *. texture_padding, 0.0)
        1
    | `Right, `Right ->
      draw_face front_facing false
        (x_max, y_min', z_min)
        (0.0, 0.0, -. 0.25 *. z_scale)
        (0.0, y_range *. texture_padding, 0.0)
        1
  in
  let draw_x_axis flip = function
    | `Bottom, `Bottom ->
      draw_face true flip
        (x_min', y_min, z_min)
        (0.0, 0.0, -. 0.25 *. z_scale)
        (x_range *. texture_padding, 0.0, 0.0)
        0
    | `Bottom, `Top ->
      draw_face false flip
        (x_min', y_min, z_max)
        (0.0, 0.0, 0.25 *. z_scale)
        (x_range *. texture_padding, 0.0, 0.0)
        0
    | `Top, `Bottom ->
      draw_face false flip
        (x_min', y_max, z_min)
        (0.0, 0.0, -. 0.25 *. z_scale)
        (x_range *. texture_padding, 0.0, 0.0)
        0
    | `Top, `Top ->
      draw_face true flip
        (x_min', y_max, z_max)
        (0.0, 0.0, 0.25 *. z_scale)
        (x_range *. texture_padding, 0.0, 0.0)
        0
  in
  let draw_z_axis flip = function
    | `Bottom, `Left ->
      draw_face false flip
        (x_min, y_min, z_min')
        (-. 0.25 *. x_scale, 0.0, 0.0)
        (0.0, 0.0, z_range *. texture_padding)
        2
    | `Bottom, `Right ->
      draw_face true flip
        (x_max, y_min, z_min')
        (0.25 *. x_scale, 0.0, 0.0)
        (0.0, 0.0, z_range *. texture_padding)
        2
    | `Top, `Left ->
      draw_face true flip
        (x_min, y_max, z_min')
        (-. 0.25 *. x_scale, 0.0, 0.0)
        (0.0, 0.0, z_range *. texture_padding)
        2
    | `Top, `Right ->
      draw_face false flip
        (x_max, y_max, z_min')
        (0.25 *. x_scale, 0.0, 0.0)
        (0.0, 0.0, z_range *. texture_padding)
        2
  in
  let pi4 = 0.25 *. Math.pi in
  let pi2 = 0.5 *. Math.pi in
  let pi6 = pi4 +. pi2 in
  begin fun angle_x angle_y ->
    let above_or_below, do_flip = if angle_x > 0.05 then `Top, false else `Bottom, true in

    if -. pi4 <= angle_y && angle_y <= 0.0 then begin
      draw_x_axis (not do_flip) (above_or_below, `Bottom);
      draw_y_axis false (`Back, `Left);
      draw_z_axis (not do_flip) (above_or_below, `Left)
    end else if -. pi2 <= angle_y && angle_y <= -. pi4 then begin
      draw_x_axis (not do_flip) (above_or_below, `Bottom);
      draw_y_axis true (`Right, `Right);
      draw_z_axis (not do_flip) (above_or_below, `Left)
    end else if -. pi6 <= angle_y && angle_y <= -. pi2 then begin
      draw_x_axis (not do_flip) (above_or_below, `Top);
      draw_y_axis false (`Right, `Left);
      draw_z_axis do_flip (above_or_below, `Left)
    end else if -. pi <= angle_y && angle_y <= -. pi6 then begin
      draw_x_axis (not do_flip) (above_or_below, `Top);
      draw_y_axis true (`Front, `Right);
      draw_z_axis do_flip (above_or_below, `Left) (* OK *)
    end else if pi >= angle_y && angle_y >= pi6 then begin
      draw_x_axis do_flip (above_or_below, `Top);  (* OK *)
      draw_y_axis false (`Front, `Left);
      draw_z_axis do_flip (above_or_below, `Right)
    end else if pi6 >= angle_y && angle_y >= pi2 then begin
      draw_x_axis do_flip (above_or_below, `Top);  (* OK *)
      draw_y_axis true (`Left, `Right);
      draw_z_axis do_flip (above_or_below, `Right)
    end else if pi2 >= angle_y && angle_y >= pi4 then begin
      draw_x_axis do_flip (above_or_below, `Bottom);
      draw_y_axis false (`Left, `Left);
      draw_z_axis (not do_flip) (above_or_below, `Right)
    end else if pi4 >= angle_y && angle_y >= 0.0 then begin
      draw_x_axis do_flip (above_or_below, `Bottom); (* OK *)
      draw_y_axis true (`Back, `Right);
      draw_z_axis (not do_flip) (above_or_below, `Right)
    end else
      assert false
  end

let create (scene : Webgl_plot_scene.scene) : #object3d =
  let gl = scene # gl in
  let texture_shader = scene # repere_shader in
  object(this)
    inherit identified
    inherit not_intersectable

    val mutable ticks = None
    val mutable cube = None

    val mutable x_min = 0.0
    val mutable x_max = 1.0

    val mutable y_min = 0.0
    val mutable y_max = 1.0

    val mutable z_min = 0.0
    val mutable z_max = 1.0

    val mutable x_axis_label = ""
    val mutable y_axis_label = ""
    val mutable z_axis_label = ""

    val mutable x_axis_ticks = []
    val mutable y_axis_ticks = []
    val mutable z_axis_ticks = []

    val mutable changed = true

    method x_axis_min = x_min
    method x_axis_max = x_max
    method y_axis_min = y_min
    method y_axis_max = y_max
    method z_axis_min = z_min
    method z_axis_max = z_max

    method private modify = changed <- true

    method set_x_axis_label s =
      this # modify; x_axis_label <- s
    method set_x_axis_ticks l =
      this # modify; x_axis_ticks <- l
    method set_x_axis_bounds (min, max) =
      this # modify; x_min <- min; x_max <- max

    method set_y_axis_label s =
      this # modify; y_axis_label <- s
    method set_y_axis_ticks l =
      this # modify; y_axis_ticks <- l
    method set_y_axis_bounds (min, max) =
      this # modify; y_min <- min; y_max <- max

    method set_z_axis_label s =
      this # modify; z_axis_label <- s
    method set_z_axis_ticks l =
      this # modify; z_axis_ticks <- l
    method set_z_axis_bounds (min, max) =
      this # modify; z_min <- min; z_max <- max

    val mutable last_scale = (0.0, 0.0, 0.0)

    method check_scale =
      if scene # scale <> last_scale then
        this # modify

    method private compute =
      delay (fun () ->
          let box = {x_min; x_max; y_min; y_max; z_min; z_max} in
          cube <- Some (build_cube gl texture_shader box);
          ticks <- Some (draw_axis gl texture_shader box (scene # scale) x_axis_label x_axis_ticks y_axis_label y_axis_ticks z_axis_label z_axis_ticks);
          changed <- false)

    method opaque = true

    method draw shader_id round =
      if shader_id = texture_shader # id && round = 0 then begin
        let angle_x, angle_y, _ = scene # angle in
        if changed then this # compute;
        (match cube with Some cube -> cube # draw | _ -> ());
        (match ticks with Some ticks -> ticks angle_x angle_y | _ -> ())
      end

    initializer scene # add (this :> object3d)

  end
