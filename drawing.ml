open Js_core
open Helper
open Html.Canvas.WebGl
open Math 

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

let red = [1.; 0.; 0.] 
let green = [0.; 1.; 0.] 
let blue = [0.; 0.; 1.]
let ones size = Array.init (3 * size) (fun k -> if k mod 3 = 2 then -1.0 else 0.0) |> Float32Array.new_float32_array 

let repere = 
 [|
   0.; 0.; 0.; 0.; 1.; 0.;
   0.; 0.; 0.; 1.; 0.; 0.;
   0.; 0.; 0.; 0.; 0.; 1.;
 |] |> Float32Array.new_float32_array

let repere_color = 
 [|
   1.; 0.; 0.; 1.; 0.; 0.;
   0.; 1.; 0.; 0.; 1.; 0.;
   0.; 0.; 1.; 0.; 0.; 1.;
 |] |> Float32Array.new_float32_array

let draw_normals = false

let draw_object gl context {size; triangles; normals; line_normals; colors; _ } = 
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
  end;

  bind_buffer gl (array_buffer gl) context.position_buffer;
  buffer_data gl (array_buffer gl) repere (static_draw gl);
  bind_buffer gl (array_buffer gl) context.normal_buffer;
  buffer_data gl (array_buffer gl) (ones 6) (static_draw gl);
  bind_buffer gl (array_buffer gl) context.color_buffer;
  buffer_data gl (array_buffer gl) repere_color (static_draw gl);
  draw_arrays gl (_LINES_ gl) 0 6


let normals_of_triangle = function 
  | [[x1; y1; z1]; [x2; y2; z2]; [x3; y3; z3]] ->
    let xn = (y2 -. y1) *. (z3 -. z1) -. (z2 -. z1) *. (y3 -. y1) in 
    let yn = (z2 -. z1) *. (x3 -. x1) -. (x2 -. x1) *. (z3 -. z1) in 
    let zn = (x2 -. x1) *. (y3 -. y1) -. (y2 -. y1) *. (x3 -. x1) in
    let n = sqrt (xn *. xn +. yn *. yn +. zn *. zn) in
    let d = [xn /. n; yn /. n; zn /. n] in
    [d;d;d]
  | _ -> assert false 

let colors_of_triangle _ =
  let c = [Random.float 1.; Random.float 1.; Random.float 1.] in
  [c;c;c]

let thing =
  let a = [ -1.; -1.;  1.; ] in
  let b = [ -1.;  1.;  1.; ] in
  let c = [  1.;  1.;  1.; ] in
  let d = [  1.; -1.;  1.; ] in
  let e = [ -1.;  1.; -1.; ] in
  let f = [  1.;  1.; -1.; ] in

  let triangles = 
    [
     [b;a;c]; 
     [c;a;d];
     [e;b;f];
     
     [c;d;f]; 
     [b;c;f];
     [a;b;e]; 

     [f;a;e]; 
     [d;a;f];
   ]
  in
  let normals = List.flatten (List.map normals_of_triangle triangles) in 
  let colors = List.flatten (List.map colors_of_triangle triangles) in
  let triangles = List.flatten triangles in
  let line_normals =
   let translate alpha a v = match a, v with 
     | [x1;y1;z1], [x2;y2;z2] ->
       [x1 +. alpha *. x2; y1 +. alpha *. y2; z1 +. alpha *. z2]
     | _ -> assert false
   in
   List.flatten (List.map2 (fun a v -> [a; translate 1.0 a v]) triangles normals) in
  let flatten_array l = Float32Array.new_float32_array (Array.of_list (List.flatten l)) in
  { 
    triangles = flatten_array triangles; 
    normals = flatten_array normals; 
    colors = flatten_array colors;
    line_normals = flatten_array line_normals;
    size = List.length triangles; 
    local_matrix = Float32Array.new_float32_array [||]
  }

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
    flatten (List.rev (List.rev_map colors_of_triangle triangles)) 
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



let red = (1.0, 0.0, 0.0)
let green = (0.0, 1.0, 0.0)
let blue = (0.0, 0.0, 1.0)

let angle = ref 0.0
let tau = 2.0 *. pi
 
let plot x y = 
  (cos (tau *. x) +. (sin (tau *. y))) /. 4.0

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

  let normal_location = 
    let attrib_location = get_attrib_location gl program "a_normal" in
    if attrib_location < 0 then
      error "unable to get 'a_normal'"
    else
      attrib_location
  in
  enable_vertex_attrib_array gl normal_location;
  let normal_buffer = create_buffer gl in
  bind_buffer gl (array_buffer gl) normal_buffer;
  vertex_attrib_pointer gl normal_location 3 (type_float gl) false 0 0;

  let color_location = 
    let attrib_location = get_attrib_location gl program "a_color" in
    if attrib_location < 0 then
      error "unable to get 'a_color'"
    else
      attrib_location
  in
  enable_vertex_attrib_array gl color_location;
  let color_buffer = create_buffer gl in
  bind_buffer gl (array_buffer gl) color_buffer;
  vertex_attrib_pointer gl color_location 3 (type_float gl) false 0 0;

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

let res = 500

let sphere50 = sphere 10 |> from_triangles
let () = print_endline "computing exp_graph ..."
let exp_graph = 
  graph 35 (-1.0) 1.0 (-1.0) 1.0 (fun x y ->
      let x = 2.0 *. x in
      let y = 2.0 *. y in
      4.0 *. exp (-. (x *. x +. y *. y))) |> (fun x -> print_endline "and all derived data ..."; x)|> from_triangles
let () = print_endline "done"

let draw_scene gl context clock =
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
  clear gl ((_COLOR_BUFFER_BIT_ gl) lor (_DEPTH_BUFFER_BIT_ gl));
  uniform_matrix4fv gl context.matrix false 
    (Float32Array.new_float32_array matrix);
  uniform3f gl context.ambient_light 0.2 0.2 0.2;
  uniform3f gl context.light_position 0. 0. (-3.);
  draw_object gl context exp_graph

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
  let gl = 
    match Html.Canvas.(get_context canvas WebGl) with
    | Some gl -> gl 
    | None -> error "webgl is not supported" 
  in

  enable gl (_DEPTH_TEST_ gl);
  depth_func gl (_LESS_ gl); 

  let vertex_shader = new_shader gl vertex_shader `Vertex in
  let fragment_shader = new_shader gl fragment_shader `Fragment in
  let program = compile_program gl vertex_shader fragment_shader in
  let open Html.Canvas.WebGl in
  use_program gl program;

  let context = setup_context gl program in
  let previous_time = ref 0.0 in
  loop (fun clock ->
    let t = clock -. !previous_time in
    previous_time := clock;
    Node.set_text_content fps (Printf.sprintf "%.2f fps" (1000.0 /. t));
    draw_scene gl context clock);
end
