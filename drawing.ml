open Js_core
open Helper
open Html
open Canvas.WebGl
open Math

module Surface = struct

  type surface = {
    triangles: (vec3 * vec3 * vec3) list;
    normals: (vec3 * vec3 * vec3) list;
    colors: (vec3 * vec3 * vec3) list;
  }

  type flat_surface = {
    size: int;

    triangles_array: Float32Array.t;
    normals_array: Float32Array.t;
    colors_array: Float32Array.t;

    bounds: Triangles.box;

    ray_table: Triangles.ray_table;
  }

  let flatten_triangles l =
    let size = List.length l in
    let array = Array.create_float (3 * 3 * size) in
    let k = ref 0 in
    List.iter (fun (a,b,c) ->
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
        k := !k + 9) l;
    Float32Array.new_float32_array array

  let flat { triangles; normals; colors} =
    let bounds = Triangles.bounding_box triangles in
    let triangles_array = flatten_triangles triangles in
    let normals_array = flatten_triangles normals in
    let colors_array = flatten_triangles colors in
    let ray_table = Triangles.build_ray_table triangles in
    { size = 3 * (List.length triangles);
      bounds; triangles_array; normals_array; colors_array; ray_table }

  let from_fun res x_min x_max y_min y_max f =
    let triangles = Param.graph res x_min x_max y_min y_max f in
    let normals =
      List.map (fun (a,b,c) ->
          let n = Triangles.normal_of_triangle a b c in
          (n, n, n)) triangles
    in
    let colors =
      List.map (fun (a, b, c) ->
          let (_, y1, _) = Vector.to_three a in
          let (_, y2, _) = Vector.to_three b in
          let (_, y3, _) = Vector.to_three c in
          let c y = Math.Color.hsv (359.9 *. (y -. y_min) /. (y_max -. y_min)) 1.0 1.0 in
          (c y1, c y2,c y3)) triangles
    in
    { triangles; normals; colors}

  let world_matrix {Triangles.x_max; x_min; y_max; y_min; z_min; z_max} (angle_x, angle_y, angle_z) =
    let open Vector in
    let open Const in
    let range_x = x_max -. x_min in
    let range_y = y_max -. y_min in
    let range_z = z_max -. z_min in

    let move_x = -. range_x /. 2.0 -. x_min in
    let move_y = -. range_y /. 2.0 -. y_min in
    let move_z = -. range_z /. 2.0 -. z_min in

    let scale_x = 1.0 /. range_x in
    let scale_y = 1.0 /. range_y in
    let scale_z = 1.0 /. range_z in

    let matrix =
      translation (of_three (move_x, move_y, move_z))
      |> multiply (scale (of_three (scale_x, scale_y, scale_z)))
      |> multiply (z_rotation angle_z)
      |> multiply (y_rotation angle_y)
      |> multiply (x_rotation angle_x)
      |> multiply projection
    in

    let matrix' =
      projection
      |> multiply (x_rotation (-. angle_x))
      |> multiply (y_rotation (-. angle_y))
      |> multiply (z_rotation (-. angle_z))
      |> multiply (scale
                     (Vector.of_three (1. /. scale_x, 1. /. scale_y, 1. /. scale_z)))
      |> multiply (translation
                     (Vector.of_three (-. move_x, -. move_y, -. move_z)))
    in
    matrix, matrix'

end

module GraphModel = struct

  type context = {
    position_location: int;
    position_buffer: float buffer;

    normal_location: int;
    normal_buffer: float buffer;

    color_location: int;
    color_buffer: float buffer;

    matrix: uniform_location;
    light_position: uniform_location;
    ambient_light: uniform_location;
  }

  type object_data = {
    triangles: Float32Array.t;
    normals: Float32Array.t;
    colors: Float32Array.t;
    line_normals: Float32Array.t;
    local_matrix: Float32Array.t;
    size: int
  }


  let vertex_shader = {gsl|
  attribute vec3 a_position;
  attribute vec3 a_normal;
  attribute vec3 a_color;
  uniform mat4 u_matrix;
  varying mediump vec3 v_position;
  varying mediump vec3 v_normal;
  varying mediump vec3 v_color;
  void main() {
    vec4 pos = u_matrix * vec4(a_position,1);
    vec4 norm = u_matrix * vec4(a_normal,1);

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
    gl_FragColor = vec4( v_color * lighting + u_ambientLight, 1);
  }
  |gsl}

  let rebind gl {position_location; position_buffer; normal_location; normal_buffer; color_location; color_buffer; } =
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
    let matrix =
      match get_uniform_location gl program "u_matrix" with
      | Some thing -> thing
      | None -> error "unable to get 'u_matrix'"
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

    { position_location; position_buffer; matrix; ambient_light; light_position; normal_location; normal_buffer; color_location; color_buffer }

  module State = struct
    let program = ref None
    let context = ref None
  end

  let context () =
    match !State.context with
    | None -> failwith "context: require init"
    | Some ctx -> ctx

  let load gl =
    match !State.program, !State.context with
    | Some program, Some context -> begin
      Html.Canvas.WebGl.use_program gl program;
      rebind gl context
    end
    | _ -> failwith "Unable to load model, require initialization first"

  let initialize gl =
    let vertex_shader = new_shader gl vertex_shader `Vertex in
    let fragment_shader = new_shader gl fragment_shader `Fragment in
    let program = compile_program gl vertex_shader fragment_shader in
    State.program := Some program;
    State.context := Some (setup_context gl program)

  let ones size =
    Array.init (3 * size) (fun k -> if k mod 3 = 2 then 1.0 else 0.0) |> Float32Array.new_float32_array

  let draw_normals = false

  let draw_point gl (x,y,z) =
    let context = context () in
    let l = 0.1 in
    let points = [| x+.l;y;z;x-.l;y;z;
                    x;y+.l;z;x;y-.l;z;
                    x;y;z+.l;x;y;z-.l |] in
    bind_buffer gl (array_buffer gl) context.position_buffer;
    buffer_data gl (array_buffer gl) (Float32Array.new_float32_array points) (static_draw gl);

    bind_buffer gl (array_buffer gl) context.normal_buffer;
    buffer_data gl (array_buffer gl) (ones (2 * 3)) (static_draw gl);

    bind_buffer gl (array_buffer gl) context.color_buffer;
    buffer_data gl (array_buffer gl) (ones (2 * 3)) (static_draw gl);

    draw_arrays gl (_LINES_ gl) 0 (2 * 3)


  let draw_object gl matrix {Surface.size; triangles_array; normals_array; colors_array; _ } =
    let context = context () in
    uniform_matrix4fv gl context.matrix false
           (Float32Array.new_float32_array matrix);
    uniform3f gl context.ambient_light 0.2 0.2 0.2;
    uniform3f gl context.light_position 0. 0. (-3.);

    disable gl (_CULL_FACE_ gl);

    bind_buffer gl (array_buffer gl) context.position_buffer;
    buffer_data gl (array_buffer gl) triangles_array (static_draw gl);

    bind_buffer gl (array_buffer gl) context.normal_buffer;
    buffer_data gl (array_buffer gl) normals_array (static_draw gl);

    bind_buffer gl (array_buffer gl) context.color_buffer;
    buffer_data gl (array_buffer gl) colors_array (static_draw gl);

    draw_arrays gl (_TRIANGLES_ gl) 0 size;
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
    enable_vertex_attrib_array gl texcoord_location;
    let texcoord_buffer = create_buffer gl in
    bind_buffer gl (array_buffer gl) texcoord_buffer;
    vertex_attrib_pointer gl texcoord_location 2 (type_float gl) false 0 0;

    let matrix =
      match get_uniform_location gl program "u_matrix" with
      | Some thing -> thing
      | None -> error "unable to get 'u_matrix'"
    in
    {
      position_location;
      position_buffer; matrix; texcoord_location; texcoord_buffer }


  module State = struct
    let program = ref None
    let context = ref None
  end

  let context () =
    match !State.context with
    | None -> failwith "context: require init"
    | Some ctx -> ctx

  let load gl =
    match !State.program with
    | None -> failwith "Unable to load model, require initialization first"
    | Some program -> begin
      Html.Canvas.WebGl.use_program gl program;
      State.context := Some (setup_context gl program)
    end

  let initialize gl =
    let vertex_shader = new_shader gl vertex_shader `Vertex in
    let fragment_shader = new_shader gl fragment_shader `Fragment in
    let program = compile_program gl vertex_shader fragment_shader in
    State.program := Some program

  type cube = {
    texture: texture;
    triangles: Float32Array.t;
    texcoords: Float32Array.t;
  }

  let build_cube {Triangles.x_min; x_max; y_min; y_max; z_min; z_max} texture =
    let triangles, texcoords =
      let tl, tr, bl, br =
        [0.; 1.], [1.; 1.],
        [0.; 0.], [1.; 0.]
      in
      let a = [x_min;y_min;z_min] in
      let b = [x_min;y_max;z_min] in
      let c = [x_max;y_max;z_min] in (*   F---G *)
      let d = [x_max;y_min;z_min] in (*  /|  /|  *)
      let e = [x_min;y_min;z_max] in (* B---C |  *)
      let f = [x_min;y_max;z_max] in (* | E-|-H  *)
      let g = [x_max;y_max;z_max] in (* |/  |/   *)
      let h = [x_max;y_min;z_max] in (* A---D    *)
      [[b,tl;a,bl;c,tr];[c,tr;a,bl;d,br]; (* front *)
       [a,bl;b,tl;f,tr];[a,bl;f,tr;e,br]; (* left *)
       [d,br;a,bl;h,tr];[h,tr;a,bl;e,tl]; (* bottom *)
       [b,bl;c,br;g,tr];[b,bl;g,tr;f,tl]; (* top *)
       [c,tl;d,bl;h,br];[c,tl;h,br;g,tr]; (* right *)
       [e,bl;f,tl;g,tr];[e,bl;g,tr;h,br]] (* back *)
      |> List.map List.split
      |> List.split
    in
    let flatten_array l =
      Float32Array.new_float32_array
        (Array.of_list (List.flatten (List.flatten l)))
    in
    ({ triangles = flatten_array triangles;
      texcoords = flatten_array texcoords;
      texture } : cube)

  let draw_cube gl {texture; texcoords; triangles} matrix =
    let context = context () in
    enable gl (_CULL_FACE_ gl);
    uniform_matrix4fv gl context.matrix false
      (Float32Array.new_float32_array matrix);
    bind_texture gl (_TEXTURE_2D_ gl) texture;
    bind_buffer gl (array_buffer gl) context.position_buffer;
    buffer_data gl (array_buffer gl) triangles (static_draw gl);
    bind_buffer gl (array_buffer gl) context.texcoord_buffer;
    buffer_data gl (array_buffer gl) texcoords (static_draw gl);
    draw_arrays gl (_TRIANGLES_ gl) 0 36;
    disable gl (_CULL_FACE_ gl)

end

let last_update = ref 0.0

let ray_casting = true

let draw_scene gl clock cube ({Surface.bounds; ray_table; _ } as surface) angle (x,y) =
  let matrix, matrix' = Surface.world_matrix bounds angle in

  clear gl ((_COLOR_BUFFER_BIT_ gl) lor (_DEPTH_BUFFER_BIT_ gl));

  RepereModel.load gl;
  RepereModel.draw_cube gl cube (matrix :> float array);

  GraphModel.load gl;
  GraphModel.draw_object gl (matrix :> float array) surface;

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
    match Triangles.ray_triangles ray_table o e with
    | Some p ->
      let (x,y,z) = Vector.to_three p in
      GraphModel.draw_point gl (x,y,z)
    | _ -> ()
  end

let loop f =
  let rec aux x =
    f x;
    Window.request_animation_frame window aux;
  in aux 0.0

let onload _ = begin
  let open Helper in
  Random.self_init ();
  let main = element_of_id "main" in
  let canvas = Document.create_html_canvas document in
  let fps = create "div" in
  Node.append_child main fps;
  configure_element  ~attributes:["width", "800"; "height", "800"] canvas;

  Node.append_child main canvas;

  let pointer = ref (0.0, 0.0) in
  let angle = ref (0., 0., 0.) in
  let moving = ref false in

  let open Event in
  add_event_listener canvas mousemove (fun evt ->
    let x = 2.0 *. (float (offsetX evt)) /. 800.0 -. 1.0 in
    let y = 1.0 -. 2.0 *. (float (offsetY evt)) /. 800.0 in
    Printf.printf "computed: %.2f %.2f\n%!" x y;
    let button = buttons evt in
    if button = 1 then begin
      if !moving then begin
        let dx, dy =
          let x', y' = !pointer in
          x -. x', y -. y'
        in
        let tx, ty, tz = !angle in
        let ty = ty +. dx in
        let tx = tx -. dy in
        angle := tx, ty, tz;
      end;
      moving := true;
    end else moving := false;
    pointer := (x,y));

  let texture_canvas = Textures.create_grid_texture document in
  Node.append_child main texture_canvas;

  let gl =
    match Html.Canvas.(get_context canvas WebGl) with
    | Some gl -> gl
    | None -> error "webgl is not supported"
  in
  enable gl (_DEPTH_TEST_ gl);
  depth_func gl (_LESS_ gl);

  RepereModel.initialize gl;
  RepereModel.load gl;

  let texture = create_texture gl in
  bind_texture gl (_TEXTURE_2D_ gl) texture;
  tex_image_2D gl (_TEXTURE_2D_ gl) 0 (_RGBA_ gl) (_RGBA_ gl) (type_unsigned_byte gl)  (`Canvas texture_canvas);
  tex_parameteri gl (_TEXTURE_2D_ gl) (_TEXTURE_MAG_FILTER_ gl) (_LINEAR_ gl);
  tex_parameteri gl (_TEXTURE_2D_ gl) (_TEXTURE_MIN_FILTER_ gl) (_LINEAR_ gl);

  let {Surface.bounds; _} as surface =
   Surface.from_fun 400 (-. pi) pi (-. pi) pi (fun x y -> sin (sqrt (x *. x +. y *. y)))
   |> Surface.flat
  in
  let cube = RepereModel.build_cube bounds texture in

  GraphModel.initialize gl;

  let previous_time = ref 0.0 in
  let fps_counter = ref 0 in
  let update_freq = 30 in
  loop (fun clock ->
    incr fps_counter;
    if !fps_counter = update_freq then begin
      let t = (clock -. !previous_time) /. (float update_freq) in
      previous_time := clock;
      fps_counter := 0;
      Node.set_text_content fps
        (Printf.sprintf "%.2f fps" (1000.0 /. t));
    end;
    draw_scene gl clock cube surface !angle !pointer);
end
