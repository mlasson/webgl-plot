open Js_core
open Helper
open Html
open Canvas.WebGl
open Math

module type MODEL = sig
  val initialize: Canvas.WebGl.t -> unit
  val load: unit -> unit
  type objects_data

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
    float lighting = max(dot(normalize(v_normal), lightDirection), 0.);
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


  let draw_object gl matrix {size; triangles; normals; line_normals; colors; _ } =
    let context = context () in
    uniform_matrix4fv gl context.matrix false
           (Float32Array.new_float32_array matrix);
    uniform3f gl context.ambient_light 0.2 0.2 0.2;
    uniform3f gl context.light_position 0. 0. (-3.);

    disable gl (_CULL_FACE_ gl);

    bind_buffer gl (array_buffer gl) context.position_buffer;
    buffer_data gl (array_buffer gl) triangles (static_draw gl);

    bind_buffer gl (array_buffer gl) context.normal_buffer;
    buffer_data gl (array_buffer gl) normals (static_draw gl);

    bind_buffer gl (array_buffer gl) context.color_buffer;
    buffer_data gl (array_buffer gl) colors (static_draw gl);

    draw_arrays gl (_TRIANGLES_ gl) 0 size;

    if draw_normals then begin
      bind_buffer gl (array_buffer gl) context.position_buffer;
      buffer_data gl (array_buffer gl) line_normals (static_draw gl);

      bind_buffer gl (array_buffer gl) context.normal_buffer;
      buffer_data gl (array_buffer gl) (ones (2 * size)) (static_draw gl);

      bind_buffer gl (array_buffer gl) context.color_buffer;
      buffer_data gl (array_buffer gl) (ones (2 * size)) (static_draw gl);

      draw_arrays gl (_LINES_ gl) 0 (2 * size)
    end

  let from_triangles triangles =
    let flatten l =
      let rec aux acc =
        function
        | [] -> List.rev acc
        | hd :: tl -> aux (List.rev_append hd acc) tl
      in aux [] l
    in
    print_endline "normals";
    let normals =
      flatten
        (List.rev
           (List.rev_map normals_of_triangle triangles))
    in
    print_endline "colors";
    let colors =
      flatten (flatten (List.rev (List.rev_map colors_of_rectangle (rectangles_of_triangles triangles))))
    in
    print_endline "flatten";
    let triangles = flatten triangles in
    print_endline "lines";
    let line_normals =
      let translate alpha a v = match a, v with
        | [x1;y1;z1], [x2;y2;z2] ->
          [x1 +. alpha *. x2; y1 +. alpha *. y2; z1 +. alpha *. z2]
        | _ -> assert false
      in
      flatten (List.rev_map2 (fun a v -> [a; translate 1.0 a v]) triangles normals)
    in
    print_endline "arrays";
    let flatten_array l =
      Float32Array.new_float32_array
        (Array.of_list (flatten l))
    in
    {
      triangles = flatten_array triangles;
      normals = flatten_array normals;
      colors = flatten_array colors;
      line_normals = flatten_array line_normals;
      size = List.length triangles;
      local_matrix = Float32Array.new_float32_array [||]
    }



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

	let fragment_shader = {gsl|
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

  let scale_triangle alpha = List.map (List.map (( *. ) alpha))

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

let cube =
  let triangles, texcoords =
    let tl, tr, bl, br =
      [0.; 1.], [1.; 1.],
      [0.; 0.], [1.; 0.]
    in
    let a = [-1.0;-1.0;-1.0] in
    let b = [-1.0; 1.0;-1.0] in
    let c = [ 1.0; 1.0;-1.0] in (*   F---G *)
    let d = [ 1.0;-1.0;-1.0] in (*  /|  /|  *)
    let e = [-1.0;-1.0; 1.0] in (* B---C |  *)
    let f = [-1.0; 1.0; 1.0] in (* | E-|-H  *)
    let g = [ 1.0; 1.0; 1.0] in (* |/  |/   *)
    let h = [ 1.0;-1.0; 1.0] in (* A---D    *)
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
  let triangles = List.map (scale_triangle 2.) triangles in
  flatten_array triangles, flatten_array texcoords

let draw_cube gl texture matrix =
  let context = context () in
  enable gl (_CULL_FACE_ gl);
  uniform_matrix4fv gl context.matrix false
       (Float32Array.new_float32_array matrix);
  bind_texture gl (_TEXTURE_2D_ gl) texture;
  bind_buffer gl (array_buffer gl) context.position_buffer;
  buffer_data gl (array_buffer gl) (fst cube) (static_draw gl);
  bind_buffer gl (array_buffer gl) context.texcoord_buffer;
  buffer_data gl (array_buffer gl) (snd cube) (static_draw gl);
  draw_arrays gl (_TRIANGLES_ gl) 0 36;
  disable gl (_CULL_FACE_ gl)


end

let () = print_endline "computing exp_graph ..."
let exp_graph_triangles =
  graph 100 (-2.0) 2.0 (-2.0) 2.0 (fun x y ->
      let x = 2.0 *. x in
      let y = 2.0 *. y in
      -1.0 +. 2.0 *. exp (-. (x *. x +. y *. y)))
let exp_graph = GraphModel.from_triangles exp_graph_triangles
let () = print_endline "done"

let draw_scene gl texture clock ((x,y) as point) =
  let angle = 0.001 *. clock in
  let alpha = 1. /. 4. in
  let matrix =
       translation (0., 0., 0.)
    |> multiply (scale (alpha, alpha, alpha))
    |> multiply (z_rotation angle)
    |> multiply (y_rotation angle)
    |> multiply (x_rotation angle)
    |> multiply (translation (0., 0., 0.))
    |> multiply make_projection
  in

  let matrix' =
       make_projection
    |> multiply (translation (0., 0., 0.))
    |> multiply (x_rotation (-. angle))
    |> multiply (y_rotation (-. angle))
    |> multiply (z_rotation (-. angle))
    |> multiply (scale (1. /. alpha, 1. /. alpha, 1. /. alpha))
    |> multiply (translation (0., 0., 0.))
  in

  let o = multiply_vector matrix' [|x;y;0.0;1.0|] in
  let e = multiply_vector matrix' [|x;y;-1.0;1.0|] in
  let o, e = match o, e with
    | [|x1;y1;z1;w1|], [|x2;y2;z2;w2|] ->
       [x1 /. w1; y1 /. w1; z1 /. w1], [x2 /. w2; y2 /. w2; z2 /. w2]
    | _ -> assert false
  in
  clear gl ((_COLOR_BUFFER_BIT_ gl) lor (_DEPTH_BUFFER_BIT_ gl));
  RepereModel.load gl;
  RepereModel.draw_cube gl texture matrix;

  let open GraphModel in begin
    load gl;
    draw_object gl matrix exp_graph;
    begin match ray_triangles o e exp_graph_triangles with
    | Some [p1;p2;p3] -> draw_point gl (p1, p2, p3)
    | _ -> ()
    end;

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
  let p = create ~text:"hello world !" "p" in
  let canvas = Document.create_html_canvas document in
  let fps = create "div" in
  Node.append_child main fps;
  configure_element  ~attributes:["width", "800"; "height", "800"] canvas;

  Node.append_child main p;
  Node.append_child main canvas;
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
print_endline "first";
  tex_parameteri gl (_TEXTURE_2D_ gl) (_TEXTURE_MAG_FILTER_ gl) (_LINEAR_ gl);
print_endline "second";
  tex_parameteri gl (_TEXTURE_2D_ gl) (_TEXTURE_MIN_FILTER_ gl) (_LINEAR_ gl);
print_endline "done";

  GraphModel.initialize gl;

  let previous_time = ref 0.0 in
  loop (fun clock ->
    let t = clock -. !previous_time in
    previous_time := clock;
    Node.set_text_content fps (Printf.sprintf "%.2f fps" (1000.0 /. t));
    draw_scene gl texture clock (0., 0.));
end
