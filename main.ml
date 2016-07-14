(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

open Js_core

let error s =
  alert ("Error: "^s);
  failwith s

let document = Window.document window

module Helper = struct
  let removeAll element =
    while
     match Node.last_child element with
     | Some child -> Node.remove_child element child; true
     | None -> false
    do () done

  let element_of_id id =
    match Document.get_element_by_id document id with
    | Some element -> element
    | None -> error (Printf.sprintf "Element of id '%s' not found" id)

  let input_of_id id =
    match Html.retype (element_of_id id) with
    | `Input input -> input
    | _ ->
      error (Printf.sprintf "Element of id '%s' should be an input element." id)

  let hide element =
    Element.set_attribute element "style" "display: none"

  let show element =
    Element.remove_attribute element "style"

  let configure_element ?text ?class_name ?style ?(attributes = []) element =
    (match text with
     | Some text -> Node.set_text_content element text
     | _ -> ());
    (match style with
     | Some style -> Element.set_attribute element "style" style
     | _ -> ());
    (match class_name with
     | Some class_name -> Element.set_class_name element class_name
     | _ -> ());
    List.iter (fun (name, value) -> Element.set_attribute element name value) attributes

  let create ?text ?class_name ?style ?attributes name =
    let element = Document.create_element document name in 
    configure_element ?text ?class_name ?style ?attributes element;
    element

  let compile_shader gl source shader_type = 
    let open Html.Canvas.WebGl in
    let shader = create_shader gl shader_type in
    shader_source gl shader source;
    compile_shader gl shader;
    let success = get_shader_parameter gl shader (shader_compile_status gl) in
    if not success then
      error (Printf.sprintf "Unable to compile shader '%s'" 
               (get_shader_info_log gl shader));
    shader

  let compile_program gl vertex_shader fragment_shader = 
    let open Html.Canvas.WebGl in
    let program = create_program gl in
    attach_shader gl program vertex_shader;
    attach_shader gl program fragment_shader;
    link_program gl program;
    let success =
      get_program_bool_parameter gl program (program_link_status gl) 
    in
    if not success then
      error (Printf.sprintf "Unable to program program '%s'"
               (get_program_info_log gl program));
    program
  
  let new_shader gl shader shader_type = 
    let shader_type =
      let open Html.Canvas.WebGl in
      match shader_type with
      | `Fragment -> fragment_shader_type gl
      | `Vertex -> vertex_shader_type gl
    in
    compile_shader gl shader shader_type

end


let vertex_shader = {gsl|
  attribute vec3 a_position;
  attribute vec3 a_normal;
  uniform mat4 u_matrix;
  varying mediump vec3 v_position;
  varying mediump vec3 v_normal;
  void main() {
    vec4 pos = u_matrix * vec4(a_position,1);
    vec4 norm = u_matrix * vec4(a_normal,1);
    v_position = pos.xyz;
    v_normal = norm.xyz;
    gl_Position = pos;
  }
|gsl}

let fragment_shader = {gsl|
  precision mediump float;
  varying vec3 v_position;
  varying vec3 v_normal;
  uniform vec3 u_lightPos;
  uniform vec3 u_ambientLight;
  void main() {
    vec3 lightDirection = normalize(u_lightPos - v_position);
    float lighting = max(dot(normalize(v_normal), lightDirection), 0.);
    vec3 col = vec3(1,1,1);
    gl_FragColor = vec4( col * lighting + u_ambientLight, 1);
  }
|gsl}

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
   0.; 0.; 1.; 0.;
   0.; 0.; 0.; 1.;
|]

open Html.Canvas.WebGl

type context = {
   position_location: int; 
   position_buffer: float buffer;

   normal_location: int; 
   normal_buffer: float buffer;

   matrix: uniform_location; 
   light_position: uniform_location;
   ambient_light: uniform_location;
 } 

type object_data = {
  triangles: Float32Array.t;
  normals: Float32Array.t;
  local_matrix: Float32Array.t;
  size: int
}

let draw_object gl context {size; triangles; normals; } = 
  bind_buffer gl (array_buffer gl) context.position_buffer;
  buffer_data gl (array_buffer gl) triangles (static_draw gl);
  bind_buffer gl (array_buffer gl) context.normal_buffer;
  buffer_data gl (array_buffer gl) normals (static_draw gl);
  draw_arrays gl (_TRIANGLES_ gl) 0 size

let normals_of_triangle = function 
  | [[x1; y1; z1]; [x2; y2; z2]; [x3; y3; z3]] ->
    let xn = (y2 -. y1) *. (z3 -. z1) -. (z2 -. z1) *. (y3 -. y1) in 
    let yn = (z2 -. z1) *. (x3 -. x1) -. (x2 -. x1) *. (z3 -. z1) in 
    let zn = (x2 -. x1) *. (y3 -. y1) -. (x2 -. x1) *. (y3 -. y1) in
    let n = sqrt (xn *. xn +. yn *. yn +. zn *. zn) in
    let d = [xn /. n; yn /. n; zn /. n] in
    [d;d;d]
  | _ -> assert false 

let thing =
  let a = [ -1.; -1.;  1.; ] in
  let b = [ -1.;  1.;  1.; ] in
  let c = [  1.;  1.;  1.; ] in
  let d = [  1.; -1.;  1.; ] in
  let e = [ -1.;  1.; -1.; ] in
  let f = [  1.;  1.; -1.; ] in
  let triangles = 
    [
     [a;b;c];
     [a;c;d];

     [b;e;f];
     [b;f;c]; 

    [c;d;f];
    [b;a;e];
    [a;f;e]; 
    [a;d;f] ]
  in
  let normals = List.flatten (List.flatten (List.map normals_of_triangle triangles)) |> Array.of_list in 
  let triangles = List.flatten (List.flatten triangles) |> Array.of_list in
  { 
    triangles = Float32Array.new_float32_array triangles; 
    normals = Float32Array.new_float32_array normals; 
    size = (Array.length triangles) / 3; local_matrix = Float32Array.new_float32_array [||]}


let red = (1.0, 0.0, 0.0)
let green = (0.0, 1.0, 0.0)
let blue = (0.0, 0.0, 1.0)

let angle = ref 0.0
let pi = 3.1415926535897932384626433832795
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

  { position_location; position_buffer; matrix; ambient_light; light_position; normal_location; normal_buffer }

let res = 500

let draw_scene gl context = 
  let matrix =
       translation (0., 0., 0.)
    |> multiply (scale (0.3, 0.3, 0.3))
    |> multiply (z_rotation !angle) 
    |> multiply (y_rotation !angle) 
    |> multiply (x_rotation !angle) 
    |> multiply make_projection 
  in
  clear gl ((_COLOR_BUFFER_BIT_ gl) lor (_DEPTH_BUFFER_BIT_ gl));
  uniform_matrix4fv gl context.matrix false 
    (Float32Array.new_float32_array matrix);
  uniform3f gl context.ambient_light 0.1 0.1 0.6;
  uniform3f gl context.light_position 1. 0. 0.;
  draw_object gl context thing

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
  let vertex_shader = new_shader gl vertex_shader `Vertex in
  let fragment_shader = new_shader gl fragment_shader `Fragment in
  let program = compile_program gl vertex_shader fragment_shader in
  let open Html.Canvas.WebGl in
  use_program gl program;
  let context = setup_context gl program in
  
  
    enable gl (_DEPTH_TEST_ gl);
  depth_func gl (_LESS_ gl); 

  let previous_time = ref 0.0 in
  loop (fun x -> 
    angle := !angle +. 0.01;
    let t = x -. !previous_time in
    previous_time := x;
    Node.set_text_content fps (Printf.sprintf "%.2f fps" (1000.0 /. t));
    draw_scene gl context);
end
let () = Window.set_onload window onload
