open Math
open Asynchronous_computations
open Js_bindings
open Helper
open Html
open Canvas.WebGl


module Surface = struct

  type surface = {
    triangles: (vec3 * vec3 * vec3) list;
    normals: (vec3 * vec3 * vec3) list;
    colors: (vec3 * vec3 * vec3) list;
    lines: (vec3 * vec3) list;
  }

  type flat_surface = {
    size: int;

    triangles_array: Float32Array.t;
    normals_array: Float32Array.t;
    colors_array: Float32Array.t;

    lines_size: int;
    lines_array: Float32Array.t;
    lines_normals_array: Float32Array.t;
    lines_colors_array: Float32Array.t;

    bounds: Triangles.box;
    ray_table: Triangles.ray_table;
  }

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
    return (Float32Array.new_float32_array array))

  let flatten_lines l =
    let size = List.length l in
    Printf.printf "flatten_lines(%d)\n" size;
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
    return (Float32Array.new_float32_array array))

  let constant_array size a =
    Printf.printf "constant_array(%d)\n" size;
    let array = Array.create_float (3 * size) in
    let k = ref 0 in
    let (x_a,y_a,z_a) = Vector.to_three a in
    range_chunks 1000 (fun _ ->
        array.(!k) <- x_a;
        array.(!k + 1) <- y_a;
        array.(!k + 2) <- z_a;
        k := !k + 3) 1 size >>= (fun () -> print_endline "done";
    return (Float32Array.new_float32_array array))

  let flat context { triangles; normals; colors; lines} =
    let bounds = Triangles.bounding_box triangles in
    context # status "Preparing data ...";
    context # push;
    context # progress 0.0;
    flatten_triangles triangles >>= fun triangles_array ->
      context # progress 0.25;
      flatten_triangles normals >>= fun normals_array ->
        context # progress 0.5;
        flatten_triangles colors >>= fun colors_array ->
          context # progress 0.65;
          flatten_lines lines >>= fun lines_array ->
            let lines_size = 2 * List.length lines in
          constant_array lines_size (Vector.of_three (0., 1., 0.)) >>= fun lines_normals_array ->
          constant_array lines_size (Vector.of_three (0., 0., 0.)) >>= fun lines_colors_array ->
            context # progress 0.75;
            Triangles.build_ray_table context triangles >>= fun ray_table ->
            context # progress 1.;
            context # pop;
            delay () >>= (fun () ->
            return { size = 3 * (List.length triangles);
             bounds; triangles_array; normals_array; colors_array; ray_table; lines_size; lines_array;
             lines_normals_array;
             lines_colors_array;
                   })

  let from_fun ?(context = console_context) res x_min x_max y_min y_max f =
    context # push;
    context # progress 0.0;
    let triangles =
      Param.graph_computation ~context res x_min x_max y_min y_max f
    in
    triangles
    >>= fun triangles ->
      let nb_triangles = List.length triangles in
      context # status "Computing the normals ...";
      context # progress (1.0 /. 3.0);
      (delay ()) >>= (fun () ->
        let cpt = ref 0 in
        let chunks = 600 in
        map_chunks ~delay:(fun () ->
            cpt := !cpt + chunks;
            context # progress (1.0 /. 3.0 +. 1.0 /. 3.0 *. (float !cpt) /. (float nb_triangles));
            delay ()
          ) chunks (fun (a,b,c) ->
            let n = Triangles.normal_of_triangle a b c in
            (n, n, n)) triangles)

    >>= fun normals ->
    context # status "Computing the colors ...";
    context # progress (2.0 /. 3.0);
    (delay ()) >>= (fun () ->
        let cpt = ref 0 in
        let chunks = 1000 in
        map_chunks ~delay:(fun () ->
              cpt := !cpt + chunks;
              context # progress (2.0 /. 3.0 +. 1.0 /.3.0 *. (float !cpt) /. (float nb_triangles));
              delay ()
            ) chunks (fun (a,b,c) ->
            let (_, y1, _) = Vector.to_three a in
            let (_, y2, _) = Vector.to_three b in
            let (_, y3, _) = Vector.to_three c in
            let c y = Math.Color.hsv (359.9 *. (y -. y_min) /. (y_max -. y_min)) 1.0 1.0 in
            (c y1, c y2,c y3)) triangles)

   >>= fun colors ->
      context # progress (3.0 /. 3.0);
      context # pop;
      return { triangles; normals; colors; lines = []}

  let from_grid_fun ?(context = console_context) res x_min x_max y_min y_max f =
    let f x y = Vector.of_three (x,f x y, y) in
    context # push;
    context # progress 0.0;
    Param.grid_of_fun ~dim1:(x_min,x_max) ~dim2:(y_min,y_max) (res, res) f
    >>= fun grid ->
    Param.triangles_of_grid grid >>= fun triangles ->
    Triangles.normal_grid grid >>= fun normal_grid ->
    Param.triangles_of_grid normal_grid >>= fun normals ->
    map_chunks 1000 (fun (a,b,c) ->
        let (_, y1, _) = Vector.to_three a in
        let (_, y2, _) = Vector.to_three b in
        let (_, y3, _) = Vector.to_three c in
        let c y = Math.Color.hsv (359.9 *. (y -. y_min) /. (y_max -. y_min)) 1.0 1.0 in
        (c y1, c y2,c y3)) triangles
    >>= fun colors ->
    Param.lines_of_grid grid >>= fun lines ->
    context # progress 1.0;
    context # pop;
    return { triangles; normals; colors; lines}


  let my_projection aspect = Vector.Const.projection ~fov:(pi /. 4.0) ~near:0.0001 ~far:10.0 ~aspect
  let my_inverse_projection aspect = Vector.Const.inverse_projection ~fov:(pi /. 4.0) ~near:0.0001 ~far:10.0 ~aspect

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
    { position_location; position_buffer; world_matrix; object_matrix; ambient_light; light_position; normal_location; normal_buffer; color_location; color_buffer }

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

  let draw_object {gl; context; _} mode world_matrix object_matrix size triangles_array normals_array colors_array =
    uniform_matrix4fv gl context.world_matrix false
           (Float32Array.new_float32_array world_matrix);
    uniform_matrix4fv gl context.object_matrix false
           (Float32Array.new_float32_array object_matrix);

    disable gl (_CULL_FACE_ gl);

    bind_buffer gl (array_buffer gl) context.position_buffer;
    buffer_data gl (array_buffer gl) triangles_array (static_draw gl);

    bind_buffer gl (array_buffer gl) context.normal_buffer;
    buffer_data gl (array_buffer gl) normals_array (static_draw gl);

    bind_buffer gl (array_buffer gl) context.color_buffer;
    buffer_data gl (array_buffer gl) colors_array (static_draw gl);

    draw_arrays gl (mode gl) 0 size

  let identity_matrix = [|1.0; 0.0; 0.0; 0.0; 0.0; 1.0; 0.0; 0.0; 0.0; 0.0; 1.0; 0.0; 0.0; 0.0; 0.0; 1.0 |]

  let draw_surface graph_model world_matrix {Surface.size; triangles_array; normals_array; colors_array; lines_array; lines_normals_array; lines_colors_array; lines_size; _ } =
    draw_object graph_model _TRIANGLES_ world_matrix identity_matrix size triangles_array normals_array colors_array;
    draw_object graph_model _LINES_ world_matrix identity_matrix lines_size lines_array lines_normals_array lines_colors_array


  let flatten_triangles l =
    Array.of_list (List.flatten (List.map (fun (a,b,c) ->
        let (x_a,y_a,z_a) = Vector.to_three a in
        let (x_b,y_b,z_b) = Vector.to_three b in
        let (x_c,y_c,z_c) = Vector.to_three c in
        [x_a;y_a;z_a;x_b;y_b;z_b;x_c;y_c;z_c]) l)) |> Float32Array.new_float32_array

  let sphere10 =
    let surface = Math.Param.sphere 20 in
    let size = 3 * List.length surface in
    let triangles = flatten_triangles surface in
    let red = Vector.of_three (1.0, 0.0, 0.0) in
    let colors = List.map (fun _ -> (red, red, red)) surface |> flatten_triangles in
    (size, triangles, colors)

  let draw_point graph_model world_matrix r pos =
    let r = Vector.scale 0.01 r in
    let object_matrix = (Vector.Const.scale_translation r pos :> float array) in
    let size, triangles_array, colors_array = sphere10 in
    draw_object graph_model _TRIANGLES_ world_matrix object_matrix size triangles_array triangles_array colors_array


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
    textures: texture array;
    triangles: Float32Array.t;
    texcoords: Float32Array.t;
  }

  let build_cube {Triangles.x_min; x_max; y_min; y_max; z_min; z_max} textures =
    let triangles, texcoords =
      let y_one = 1.0 -. 1.0 /. 1024.0 in
      let x_one = y_one /. 8.0 in
      let tl, tr, bl, br =
        [0.; 0.], [x_one; 0.],
        [0.; y_one], [x_one; y_one]
      in
      Printf.printf "x_min: %f, x_max: %f, y_min: %f, y_max: %f, z_min: %f, z_max:%f\n%!" x_min x_max y_min y_max z_min z_max;
      let x_range = x_max -. x_min in
      let x_min = x_min -. x_range in
      let x_max = x_max +. x_range in
      let y_range = y_max -. y_min in
      let y_min = y_min -. y_range in
      let y_max = y_max +. y_range in
      let z_range = z_max -. z_min in
      let z_min = z_min -. z_range in
      let z_max = z_max +. z_range in
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
      let translate (v, s) = List.map (List.map (fun (a,t) -> List.map2 (+.) a v, List.map2 (+.) s t)) in
      let shift = 1.0 /. 8.0 in
      let delta = 1.0 in
      let f2b = [0.0;0.0; delta *. z_range], [shift *. 0.0; 0.0] in
      let l2r = [delta *. x_range;0.0;0.0], [shift *. 1.0; 0.0] in
      let b2t = [0.0;delta *. y_range;0.0], [shift *. 2.0; 0.0] in

      let b2f = [0.0;0.0; -. z_range *. delta], [shift *. 3.0; 0.0] in
      let r2l = [-. x_range *. delta ;0.0;0.0], [shift *. 4.0; 0.0] in
      let t2b = [0.0;-. y_range *. delta;0.0], [shift *. 5.0; 0.0] in
      [ translate f2b front;
        translate l2r left;
        translate b2t bottom;
        translate t2b top;
        translate r2l right;
        translate b2f back; ]
      |> List.flatten
      |> List.map List.split
      |> List.split
    in
    let flatten_array l =
      Float32Array.new_float32_array
        (Array.of_list (List.flatten (List.flatten l)))
    in
    ({ triangles = flatten_array triangles;
      texcoords = flatten_array texcoords;
      textures } : cube)

  let draw_cube {gl; context; _} {textures; texcoords; triangles} texture_index matrix =
    enable gl (_CULL_FACE_ gl);
    enable gl (_BLEND_ gl);
    depth_mask gl false;
    blend_func gl (_SRC_ALPHA_ gl) (_ONE_MINUS_SRC_ALPHA_ gl);
    uniform_matrix4fv gl context.matrix false
      (Float32Array.new_float32_array matrix);
    bind_texture gl (_TEXTURE_2D_ gl) textures.(texture_index);
    bind_buffer gl (array_buffer gl) context.position_buffer;
    buffer_data gl (array_buffer gl) triangles (static_draw gl);
    bind_buffer gl (array_buffer gl) context.texcoord_buffer;
    buffer_data gl (array_buffer gl) texcoords (static_draw gl);
    draw_arrays gl (_TRIANGLES_ gl) 0 36;
    depth_mask gl true;
    disable gl (_BLEND_ gl);
    disable gl (_CULL_FACE_ gl)

end


let last_update = ref 0.0

let draw_scene gl aspect repere_model graph_model clock cube ({Surface.bounds; ray_table; _ } as surface) ((angle_x, angle_y, _) as angle) move scale (x,y) =
  let proportions, matrix, matrix' = Surface.world_matrix aspect bounds angle move scale in

  clear gl ((_COLOR_BUFFER_BIT_ gl) lor (_DEPTH_BUFFER_BIT_ gl));

  RepereModel.load repere_model;
  let pi2 = pi /. 2.0 in
  let pi4 = pi /. 4.0 in
  let texture_index =
    let pi6 = 0.75 *. pi in
    Printf.printf "angle_x = %1.2f angle_y = %1.2f\n%!" angle_x angle_y;
    if -. pi4 <= angle_y && angle_y <= 0.0 then
      0
    else if -. pi2 <= angle_y && angle_y <= -. pi4 then
      1
    else if -. pi6 <= angle_y && angle_y <= -. pi2 then
      2
    else if -. pi <= angle_y && angle_y <= -. pi6 then
      3
    else if pi >= angle_y && angle_y >= pi6 then
      4
    else if pi6 >= angle_y && angle_y >= pi2 then
      5
    else if pi2 >= angle_y && angle_y >= pi4 then
      6
    else if pi4 >= angle_y && angle_y >= 0.0 then
      7
    else
      0 (* should not happen *)
  in
  let texture_index = if angle_x > 0.05 then texture_index + 8 else texture_index in
  RepereModel.draw_cube repere_model cube texture_index (matrix :> float array);

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
    match Triangles.ray_triangles ray_table o e with
    | Some p ->
      GraphModel.draw_point graph_model (matrix :> float array) proportions p
    | _ -> ()
  end;
  GraphModel.draw_surface graph_model (matrix :> float array) surface
