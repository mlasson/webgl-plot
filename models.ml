open Math
open Asynchronous_computations
open Js_bindings
open Helper
open Html
open Canvas.WebGl


module Surface = struct

  type object_description = {
    size: int;

    points: Float32Array.t;
    colors: Float32Array.t;
    normals: Float32Array.t;

    indexes: Uint16Array.t;
  }

  type surface = {
    mesh : object_description;
    lines : object_description;

    bounds: Triangles.box;
    ray_table: Triangles.ray_table;
  }

  let matrix_dimension mat =
    let dim1 = Array.length mat in
    if dim1 = 0 then (0,0) else
    (dim1, Array.length mat.(0))

  let flatten_matrix mat =
    let dim1, dim2 = matrix_dimension mat in
    let result = Float32Array.new_float32_array (`Size (dim1 * dim2 * 3)) in
    let k = ref 0 in
    Array.iter (Array.iter (fun v ->
        let (x,y,z) = Vector.to_three v in
        Float32Array.set result !k x; incr k;
        Float32Array.set result !k y; incr k;
        Float32Array.set result !k z; incr k;
      )) mat;
    result

  let from_grid context (points, colors) =
    Triangles.normal_grid points >>= fun normals ->
    let dim1, dim2 = matrix_dimension points in
    let normals = flatten_matrix normals in
    let points = flatten_matrix points in
    let colors = flatten_matrix colors in
    let indexes = Triangles.triangles_indexes_from_grid dim1 dim2 in
    let mesh =
      {
        size = Uint16Array.length indexes;
        points;
        colors;
        normals;
        indexes;
      }
    in
    let lines =
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
       let colors = Float32Array.new_float32_array (`Copy colors) in
       Float32Array.update colors (fun _ _ -> 0.0);
      {
        size;

        points;
        colors;
        normals;

        indexes = Uint16Array.new_uint16_array (`Data segments);
      }
    in
    let bounds = Triangles.bounding_box points in
    Triangles.build_ray_table context points indexes >>= fun ray_table ->
    return { lines; mesh; bounds; ray_table }


  let flatten_triangles l =
    let size = List.length l in
    let array = Array.create_float (3 * 3 * size) in
    let k = ref 0 in
    iter_chunks 1000 (fun (a,b,c) ->
        let (x_a,y_a,z_a) = Vector.to_three a in
        let (x_b,y_b,z_b) = Vector.to_three b in
        let (x_c,y_c,z_c) = Vector.to_three c in
        array.(!k) <- x_a;
        array.(!k + 1) <- y_a;
        array.(!k + 2) <- z_a;
        array.(!k + 3) <- x_b;
        array.(!k + 4) <- y_b;
        array.(!k + 5) <- z_b;
        array.(!k + 6) <- x_c;
        array.(!k + 7) <- y_c;
        array.(!k + 8) <- z_c;
        k := !k + 9) l >>= (fun () ->
    return (Float32Array.new_float32_array (`Data array)))

  let flatten_lines l =
    let size = List.length l in
    let array = Array.create_float (2 * 3 * size) in
    let k = ref 0 in
    iter_chunks 1000 (fun (a,b) ->
        let (x_a,y_a,z_a) = Vector.to_three a in
        let (x_b,y_b,z_b) = Vector.to_three b in
        array.(!k) <- x_a;
        array.(!k + 1) <- y_a;
        array.(!k + 2) <- z_a;
        array.(!k + 3) <- x_b;
        array.(!k + 4) <- y_b;
        array.(!k + 5) <- z_b;
        k := !k + 6) l >>= (fun () ->
    return (Float32Array.new_float32_array (`Data array)))

  let constant_array size a =
    let array = Array.create_float (3 * size) in
    let k = ref 0 in
    let (x_a,y_a,z_a) = Vector.to_three a in
    range_chunks 1000 (fun _ ->
        array.(!k) <- x_a;
        array.(!k + 1) <- y_a;
        array.(!k + 2) <- z_a;
        k := !k + 3) 1 size >>= (fun () -> print_endline "done";
    return (Float32Array.new_float32_array (`Data array)))

  let from_grid_fun ?(context = console_context) res x_min x_max y_min y_max f =
    let f x y = Vector.of_three (x,f x y, y) in
    context # push;
    context # progress 0.0;
    Param.grid_of_fun ~dim1:(x_min,x_max) ~dim2:(y_min,y_max) (res, res) f
    >>= fun points ->
    let colors =
      let y_min, y_max =
        if Array.length points = 0 || Array.length points.(0) = 0 then -1.0, 1.0 else
          begin
            let _, y, _ = Vector.to_three points.(0).(0) in
            let y_min = ref y in
            let y_max = ref y in
            Array.iter (Array.iter (fun v ->
                let _, y, _ = Vector.to_three v in
                if y < !y_min then y_min := y;
                if y > !y_max then y_max := y)) points;
            !y_min, !y_max
          end
      in
      Array.map (Array.map (fun v ->
          let _, y, _ = Vector.to_three v in
          Math.Color.hsv (359.9 *. (y -. y_min) /. (y_max -. y_min)) 1.0 1.0
        )) points
    in
    context # progress 1.0;
    context # pop;
    return (points, colors)

  let my_projection aspect = Vector.Const.projection ~fov:(pi /. 4.0) ~near:0.001 ~far:4.0 ~aspect
  let my_inverse_projection aspect = Vector.Const.inverse_projection ~fov:(pi /. 4.0) ~near:0.001 ~far:4.0 ~aspect

  let world_matrix aspect {Triangles.x_max; x_min; y_max; y_min; z_min; z_max} (angle_x, angle_y, angle_z) (trans_x, trans_y, trans_z) scale_xyz =
    let open Vector in
    let open Const in
    let range_x = x_max -. x_min in
    let range_y = y_max -. y_min in
    let range_z = z_max -. z_min in

    let move_x = -. range_x /. 2.0 -. x_min in
    let move_y = -. range_y /. 2.0 -. y_min in
    let move_z = -. range_z /. 2.0 -. z_min in

    let scale_x = scale_xyz /. range_x in
    let scale_y = scale_xyz /. range_y in
    let scale_z = scale_xyz /. range_z in

    let matrix =
      translation (of_three (move_x, move_y, move_z))
      |> multiply (scale (of_three (scale_x, scale_y, scale_z)))
      |> multiply (z_rotation angle_z)
      |> multiply (y_rotation angle_y)
      |> multiply (x_rotation angle_x)
      |> multiply (translation (of_three (trans_x, trans_y, trans_z)))
      |> multiply flip
      |> multiply (my_projection aspect)
    in

    let proportions = of_three (1.0 /. scale_x, 1.0 /. scale_y, 1.0 /. scale_z) in

    let matrix' =
      (my_inverse_projection aspect)
      |> multiply flip
      |> multiply (translation (of_three (-. trans_x, -. trans_y, -. trans_z)))
      |> multiply (x_rotation (-. angle_x))
      |> multiply (y_rotation (-. angle_y))
      |> multiply (z_rotation (-. angle_z))
      |> multiply (scale proportions)
      |> multiply (translation
                     (of_three (-. move_x, -. move_y, -. move_z)))
    in
    proportions, matrix, matrix'

end

module GraphModel = struct

  type context = {
    position_location: int;
    position_buffer: float buffer;

    normal_location: int;
    normal_buffer: float buffer;

    color_location: int;
    color_buffer: float buffer;

    index_buffer: int buffer;

    world_matrix: uniform_location;
    object_matrix: uniform_location;
    light_position: uniform_location;
    ambient_light: uniform_location;
  }

  let vertex_shader = {gsl|

  attribute vec3 a_position;
  attribute vec3 a_normal;
  attribute vec3 a_color;

  uniform mat4 u_world_matrix;
  uniform mat4 u_object_matrix;


  varying mediump vec3 v_position;
  varying mediump vec3 v_normal;
  varying mediump vec3 v_color;

  void main() {

    vec4 pos = u_world_matrix * u_object_matrix * vec4(a_position,1);
    vec4 norm = u_world_matrix * u_object_matrix * vec4(a_normal,1);

    v_position = pos.xyz;
    v_normal = norm.xyz;
    v_color = a_color.xyz;

    gl_Position = pos;
  }
  |gsl}

  let fragment_shader = {gsl|
  precision mediump float;

  varying vec3 v_position;
  varying vec3 v_normal;
  varying vec3 v_color;

  uniform vec3 u_lightPos;
  uniform vec3 u_ambientLight;

  void main() {
    vec3 lightDirection = normalize(u_lightPos - v_position);
    float lighting = abs(dot(normalize(v_normal), lightDirection));
    gl_FragColor = vec4( v_color * (lighting + u_ambientLight), 1);
  }
  |gsl}

  let rebind gl {position_location; position_buffer; normal_location; normal_buffer; color_location; color_buffer; _ } =
    enable_vertex_attrib_array gl position_location;
    bind_buffer gl (array_buffer gl) position_buffer;
    vertex_attrib_pointer gl position_location 3 (type_float gl) false 0 0;
    enable_vertex_attrib_array gl normal_location;
    bind_buffer gl (array_buffer gl) normal_buffer;
    vertex_attrib_pointer gl normal_location 3 (type_float gl) false 0 0;
    enable_vertex_attrib_array gl color_location;
    bind_buffer gl (array_buffer gl) color_buffer;
    vertex_attrib_pointer gl color_location 3 (type_float gl) false 0 0


  let setup_context gl program =
    let position_location =
      let attrib_location = get_attrib_location gl program "a_position" in
      if attrib_location < 0 then
        error "unable to get 'a_position'"
      else
        attrib_location
    in
    let position_buffer = create_buffer gl in
    let normal_location =
      let attrib_location = get_attrib_location gl program "a_normal" in
      if attrib_location < 0 then
        error "unable to get 'a_normal'"
      else
        attrib_location
    in
    let normal_buffer = create_buffer gl in
    let color_location =
      let attrib_location = get_attrib_location gl program "a_color" in
      if attrib_location < 0 then
        error "unable to get 'a_color'"
      else
        attrib_location
    in
    let color_buffer = create_buffer gl in
    let world_matrix =
      match get_uniform_location gl program "u_world_matrix" with
      | Some thing -> thing
      | None -> error "unable to get 'u_world_matrix'"
    in
    let object_matrix =
      match get_uniform_location gl program "u_object_matrix" with
      | Some thing -> thing
      | None -> error "unable to get 'u_object_matrix'"
    in
    let ambient_light =
      match get_uniform_location gl program "u_ambientLight" with
      | Some thing -> thing
      | None -> error "unable to get 'u_ambientLight'"
    in
    let light_position =
      match get_uniform_location gl program "u_lightPos" with
      | Some thing -> thing
      | None -> error "unable to get 'u_lightPos'"
    in
    let index_buffer = create_buffer gl in
    { position_location; position_buffer; world_matrix; object_matrix; ambient_light; light_position; normal_location; normal_buffer; color_location; color_buffer; index_buffer }

  type state = {
    program: program;
    gl: Html.Canvas.WebGl.t;
    context: context;
  }

  let load {gl; program; context} =
    Html.Canvas.WebGl.use_program gl program;
    rebind gl context

  let initialize gl =
    let vertex_shader = new_shader gl vertex_shader `Vertex in
    let fragment_shader = new_shader gl fragment_shader `Fragment in
    let program = compile_program gl vertex_shader fragment_shader in
    {gl; program; context = setup_context gl program}

  let set_light {gl; context; _} (x, y, z) =
    uniform3f gl context.ambient_light 0.2 0.2 0.2;
    uniform3f gl context.light_position x y z

  let draw_object {gl; context; _} mode world_matrix object_matrix {Surface.size; points; colors; normals; indexes; } =
    uniform_matrix4fv gl context.world_matrix false
           (Float32Array.new_float32_array (`Data world_matrix));
    uniform_matrix4fv gl context.object_matrix false
           (Float32Array.new_float32_array (`Data object_matrix));

    disable gl (_CULL_FACE_ gl);

    bind_buffer gl (array_buffer gl) context.position_buffer;
    buffer_data gl (array_buffer gl) points (static_draw gl);

    bind_buffer gl (array_buffer gl) context.normal_buffer;
    buffer_data gl (array_buffer gl) normals (static_draw gl);

    bind_buffer gl (array_buffer gl) context.color_buffer;
    buffer_data gl (array_buffer gl) colors (static_draw gl);

    bind_buffer gl (element_array_buffer gl) context.index_buffer;
    buffer_data gl (element_array_buffer gl) indexes (static_draw gl);

    draw_elements gl (mode gl) size (type_unsigned_short gl) 0

  let identity_matrix = [|1.0; 0.0; 0.0; 0.0; 0.0; 1.0; 0.0; 0.0; 0.0; 0.0; 1.0; 0.0; 0.0; 0.0; 0.0; 1.0 |]

  let draw_surface graph_model world_matrix { Surface.mesh; lines; _ } =
    draw_object graph_model _TRIANGLES_ world_matrix identity_matrix mesh;
    draw_object graph_model _LINES_ world_matrix identity_matrix lines


  let flatten_triangles l =
   `Data (Array.of_list (List.flatten (List.map (fun (a,b,c) ->
        let (x_a,y_a,z_a) = Vector.to_three a in
        let (x_b,y_b,z_b) = Vector.to_three b in
        let (x_c,y_c,z_c) = Vector.to_three c in
        [x_a;y_a;z_a;x_b;y_b;z_b;x_c;y_c;z_c]) l))) |> Float32Array.new_float32_array

  let sphere10 =
    let surface = Math.Param.sphere 20 in
    let size = 3 * List.length surface in
    let points = flatten_triangles surface in
    let color = Vector.of_three (0.0, 0.0, 0.0) in
    let colors = List.map (fun _ -> (color, color, color)) surface |> flatten_triangles in
    let indexes = Uint16Array.new_uint16_array (`Data (Array.init size (fun x -> x))) in
    {Surface.size ; points; colors; normals = points; indexes; }

  let draw_point graph_model world_matrix r pos =
    let r = Vector.scale 0.01 r in
    let object_matrix = (Vector.Const.scale_translation r pos :> float array) in
    draw_object graph_model _TRIANGLES_ world_matrix object_matrix sphere10


end



module RepereModel= struct
  let vertex_shader =
  {gsl|
        attribute vec3 a_position;
        attribute vec2 a_texcoord;
        uniform mat4 u_matrix;
        varying vec2 v_texcoord;
        void main() {
          gl_Position = u_matrix * vec4(a_position,1);
          v_texcoord = a_texcoord;
        }
  |gsl}

  let fragment_shader =
  {gsl|
        precision mediump float;
        varying vec2 v_texcoord;
        uniform sampler2D u_texture;
        void main() {
          gl_FragColor = texture2D(u_texture, v_texcoord);
        }
  |gsl}

  type context = {
    position_location: int;
    position_buffer: float buffer;
    texcoord_location: int;
    texcoord_buffer: float buffer;
    matrix: uniform_location;
  }

  let setup_context gl program =
    let position_location =
      let attrib_location = get_attrib_location gl program "a_position" in
      if attrib_location < 0 then
        error "unable to get 'a_position'"
      else
        attrib_location
    in
    enable_vertex_attrib_array gl position_location;
    let position_buffer = create_buffer gl in
    bind_buffer gl (array_buffer gl) position_buffer;
    vertex_attrib_pointer gl position_location 3 (type_float gl) false 0 0;

    let texcoord_location =
      let attrib_location = get_attrib_location gl program "a_texcoord" in
      if attrib_location < 0 then
        error "unable to get 'a_texcoord'"
      else
        attrib_location
    in
    let texcoord_buffer = create_buffer gl in
    let matrix =
      match get_uniform_location gl program "u_matrix" with
      | Some thing -> thing
      | None -> error "unable to get 'u_matrix'"
    in
    {
      position_location;
      position_buffer; matrix; texcoord_location; texcoord_buffer }

  let rebind gl {position_location; position_buffer; texcoord_location; texcoord_buffer; _} =
    enable_vertex_attrib_array gl position_location;
    bind_buffer gl (array_buffer gl) position_buffer;
    vertex_attrib_pointer gl position_location 3 (type_float gl) false 0 0;
    enable_vertex_attrib_array gl texcoord_location;
    bind_buffer gl (array_buffer gl) texcoord_buffer;
    vertex_attrib_pointer gl texcoord_location 2 (type_float gl) false 0 0;

  type state = {
    program: program;
    gl: Html.Canvas.WebGl.t;
    context: context;
  }

  let load {gl; program; context} =
    Html.Canvas.WebGl.use_program gl program;
    rebind gl context

  let initialize gl =
    let vertex_shader = new_shader gl vertex_shader `Vertex in
    let fragment_shader = new_shader gl fragment_shader `Fragment in
    let program = compile_program gl vertex_shader fragment_shader in
    {gl; program; context = setup_context gl program}

  type cube = {
    texture: texture;
    triangles: Float32Array.t;
    texcoords: Float32Array.t;
    size: int;
  }

  let build_cube {Triangles.x_min; x_max; y_min; y_max; z_min; z_max} texture =
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
    ({ triangles = flatten_array triangles;
      texcoords = flatten_array texcoords;
      size = 36;
      texture } : cube)

  let draw_cube {gl; context; _} {texture; texcoords; triangles; size} matrix =
    enable gl (_CULL_FACE_ gl);
    enable gl (_BLEND_ gl);
    depth_mask gl false;
    blend_func gl (_SRC_ALPHA_ gl) (_ONE_MINUS_SRC_ALPHA_ gl);
    uniform_matrix4fv gl context.matrix false
      (Float32Array.new_float32_array (`Data matrix));
    bind_texture gl (_TEXTURE_2D_ gl) texture;
    bind_buffer gl (array_buffer gl) context.position_buffer;
    buffer_data gl (array_buffer gl) triangles (static_draw gl);
    bind_buffer gl (array_buffer gl) context.texcoord_buffer;
    buffer_data gl (array_buffer gl) texcoords (static_draw gl);
    draw_arrays gl (_TRIANGLES_ gl) 0 size;
    depth_mask gl true;
    disable gl (_BLEND_ gl);
    disable gl (_CULL_FACE_ gl)

end


let last_update = ref 0.0

let face_coord flip reverse texture direction =
  let face_coord x_zero x_one =
    let y_one = if reverse then 0.0 else 1.0 in
    let y_zero = if reverse then 1.0 else 0.0 in
    Vector.of_two (x_zero, y_one),
    Vector.of_two (x_zero, y_zero),
    Vector.of_two (x_one, y_one),
    Vector.of_two (x_one, y_zero)
  in
  let x_min =
    let direction =
      if flip then
        match direction with
          `Left -> `Right
        | _ -> `Left
      else direction
    in
    match texture, direction with
    | `Increasing, `Left  -> 0.0
    | `Increasing, `Right -> 0.25
    | `Decreasing, `Left -> 0.5
    | `Decreasing, `Right -> 0.75
  in
  let x_max = x_min +. 0.25 in
  if flip then
    face_coord x_max x_min
  else
    face_coord x_min x_max

let draw_faces repere_model {Triangles.x_min; x_max; y_min; y_max; z_max; z_min} face_textures matrix angle_x angle_y =
  let draw_face front_facing flip a u v texture =
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
    RepereModel.draw_cube repere_model {triangles; texcoords; texture; size = 6} matrix
  in
  let x_range = x_max -. x_min in
  let y_range = y_max -. y_min in
  let z_range = z_max -. z_min in

  let texture_padding = 1.0 /. 0.9 in
  let y_min' = y_min -. 0.5 *. y_range *.  (texture_padding -. 1.) in
  let x_min' = x_min -. 0.5 *. x_range *.  (texture_padding -. 1.) in
  let z_min' = z_min -. 0.5 *. z_range *.  (texture_padding -. 1.) in

  let draw_y_axis front_facing = function
    | `Back, `Left ->
      draw_face front_facing false
        (x_min, y_min', z_max)
        (-. 0.25 *. x_range, 0.0, 0.0)
        (0.0, y_range *. texture_padding, 0.0)
        face_textures.(1)
    | `Back, `Right ->
      draw_face front_facing false
        (x_max, y_min', z_max)
        (0.25 *. x_range, 0.0, 0.0)
        (0.0, y_range *. texture_padding, 0.0)
        face_textures.(1)
    | `Front, `Left ->
      draw_face front_facing false
        (x_max, y_min', z_min)
        (0.25 *. x_range, 0.0, 0.0)
        (0.0, y_range *. texture_padding, 0.0)
        face_textures.(1)
    | `Front, `Right ->
      draw_face front_facing false
        (x_min, y_min', z_min)
        (-. 0.25 *. x_range, 0.0, 0.0)
        (0.0, y_range *. texture_padding, 0.0)
        face_textures.(1)
    | `Left, `Left ->
      draw_face front_facing false
        (x_min, y_min', z_min)
        (0.0, 0.0, -. 0.25 *. z_range)
        (0.0, y_range *. texture_padding, 0.0)
        face_textures.(1)
    | `Left, `Right ->
      draw_face front_facing false
        (x_min, y_min', z_max)
        (0.0, 0.0, 0.25 *. z_range)
        (0.0, y_range *. texture_padding, 0.0)
        face_textures.(1)
    | `Right, `Left ->
      draw_face front_facing false
        (x_max, y_min', z_max)
        (0.0, 0.0, 0.25 *. z_range)
        (0.0, y_range *. texture_padding, 0.0)
        face_textures.(1)
    | `Right, `Right ->
      draw_face front_facing false
        (x_max, y_min', z_min)
        (0.0, 0.0, -. 0.25 *. z_range)
        (0.0, y_range *. texture_padding, 0.0)
        face_textures.(1)
  in
  let draw_x_axis flip = function
    | `Bottom, `Bottom ->
      draw_face true flip
        (x_min', y_min, z_min)
        (0.0, 0.0, -. 0.25 *. z_range)
        (x_range *. texture_padding, 0.0, 0.0)
        face_textures.(0)
    | `Bottom, `Top ->
      draw_face false flip
        (x_min', y_min, z_max)
        (0.0, 0.0, 0.25 *. z_range)
        (x_range *. texture_padding, 0.0, 0.0)
        face_textures.(0)
    | `Top, `Bottom ->
      draw_face false flip
        (x_min', y_max, z_min)
        (0.0, 0.0, -. 0.25 *. z_range)
        (x_range *. texture_padding, 0.0, 0.0)
        face_textures.(0)
    | `Top, `Top ->
      draw_face true flip
        (x_min', y_max, z_max)
        (0.0, 0.0, 0.25 *. z_range)
        (x_range *. texture_padding, 0.0, 0.0)
        face_textures.(0)
  in
  let draw_z_axis flip = function
    | `Bottom, `Left ->
      draw_face false flip
        (x_min, y_min, z_min')
        (-. 0.25 *. x_range, 0.0, 0.0)
        (0.0, 0.0, z_range *. texture_padding)
        face_textures.(2)
    | `Bottom, `Right ->
      draw_face true flip
        (x_max, y_min, z_min')
        (0.25 *. x_range, 0.0, 0.0)
        (0.0, 0.0, z_range *. texture_padding)
        face_textures.(2)
    | `Top, `Left ->
      draw_face true flip
        (x_min, y_max, z_min')
        (-. 0.25 *. x_range, 0.0, 0.0)
        (0.0, 0.0, z_range *. texture_padding)
        face_textures.(2)
    | `Top, `Right ->
      draw_face false flip
        (x_max, y_max, z_min')
        (0.25 *. x_range, 0.0, 0.0)
        (0.0, 0.0, z_range *. texture_padding)
        face_textures.(2)
  in
  let pi4 = 0.25 *. Math.pi in
  let pi2 = 0.5 *. Math.pi in
  let pi6 = pi4 +. pi2 in

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

let draw_scene gl aspect repere_model graph_model clock cube face_textures ({Surface.bounds; ray_table; _ } as surface) ((angle_x, angle_y, _) as angle) move scale (x,y) callback =
  let proportions, matrix, matrix' = Surface.world_matrix aspect bounds angle move scale in

  if Math.debug then begin
    let x1, y1, z1 = angle in
    let x2, y2, z2 = move in
    Printf.printf "angle = (%f, %f, %f), move = (%f, %f, %f)\n%!" x1 y1 z1 x2 y2 z2;
  end;

  clear gl ((_COLOR_BUFFER_BIT_ gl) lor (_DEPTH_BUFFER_BIT_ gl));

  RepereModel.load repere_model;
  RepereModel.draw_cube repere_model cube (matrix :> float array);
  draw_faces repere_model bounds face_textures (matrix :> float array) angle_x angle_y;

  GraphModel.load graph_model;

  if clock -. !last_update > 0.2 then begin
    let open Vector in
    let o =
      four_to_three
        (multiply_vector matrix' (Vector.of_four (x,y,1.0,1.0)))
    in
    let e =
      four_to_three
        (multiply_vector matrix' (Vector.of_four (x,y,-1.0,1.0)))
    in
    last_update := clock;
    GraphModel.set_light graph_model (1.0,1.0,-2.0);
    let intersection = Triangles.ray_triangles surface.mesh.points ray_table o e  in
    (match intersection with
    | Some p ->
      GraphModel.draw_point graph_model (matrix :> float array) proportions p
    | _ -> ());
    callback (x,y) intersection
  end;
  GraphModel.draw_surface graph_model (matrix :> float array) surface
