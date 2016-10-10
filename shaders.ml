open Helper
open Webgl
open Webgl.Constant

let compile_shader gl source shader_type =
  let shader = create_shader gl shader_type in
  shader_source gl shader source;
  compile_shader gl shader;
  let success = get_shader_bool_parameter gl shader _COMPILE_STATUS_ in
  if not success then
    error (Printf.sprintf "Unable to compile shader '%s'"
             (get_shader_info_log gl shader));
  shader

let compile_program gl vertex_shader fragment_shader =
  let program = create_program gl in
  attach_shader gl program vertex_shader;
  attach_shader gl program fragment_shader;
  link_program gl program;
  let success =
    get_program_bool_parameter gl program _LINK_STATUS_
  in
  if not success then
    error (Printf.sprintf "Unable to program program '%s'"
             (get_program_info_log gl program));
  program

let new_shader gl shader shader_type =
  let shader_type =
    let open Webgl in
    let open Constant in
    match shader_type with
    | `Fragment -> _FRAGMENT_SHADER_
    | `Vertex -> _VERTEX_SHADER_
  in
  compile_shader gl shader shader_type

let get_and_enable_vertex_attrib_array_location gl program location =
  let attrib_location = get_attrib_location gl program location in
  if attrib_location < 0 then
    error (Printf.sprintf "unable to get '%s'" location);
  enable_vertex_attrib_array gl attrib_location;
  attrib_location

class attrib_array gl dim =
  let buffer = create_buffer gl in
  object(this)
    val mutable count = 0
    method count = count
    method dim = dim
    method fill data =
      let size = Float32Array.length data in
      assert (size mod dim = 0);
      count <- size / dim;
      this # bind;
      buffer_data gl _ARRAY_BUFFER_ (Float32Array.t_to_js data) _STATIC_DRAW_
    method private bind =
      bind_buffer gl _ARRAY_BUFFER_ buffer
    method plug location =
      this # bind;
      vertex_attrib_pointer gl location dim _FLOAT_ false 0 0
  end

let create_attrib_array gl dim data =
  let o = new attrib_array gl dim in
  o # fill data;
  o

class element_array gl =
  let buffer = create_buffer gl in
  object(this)
    val mutable index_type = _UNSIGNED_BYTE_
    val mutable size = 0
    method index_type = index_type
    method size = size
    method buffer = buffer
    method fill (data : Geometry.Index.t) =
      let data =
        match data with
        | `Byte data ->
          index_type <- _UNSIGNED_BYTE_;
          size <- Uint8Array.length data;
          Uint8Array.t_to_js data
        | `Short data ->
          index_type <- _UNSIGNED_SHORT_;
          size <- Uint16Array.length data;
          Uint16Array.t_to_js data
        | `Int data ->
          index_type <- _UNSIGNED_INT_;
          size <- Uint32Array.length data;
          Uint32Array.t_to_js data
      in
      this # bind;
      buffer_data gl _ELEMENT_ARRAY_BUFFER_ data _STATIC_DRAW_
    method bind =
      bind_buffer gl _ELEMENT_ARRAY_BUFFER_ buffer
  end

let create_element_array gl data =
  let o = new element_array gl in
  o # fill data;
  o

type mode =
  | Triangles
  | Lines

let constant_of_mode = function
  | Triangles -> _TRIANGLES_
  | Lines -> _LINES_

module Basic = struct
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
    gl_FragColor = vec4( v_color * (0.3 * lighting + 0.7 * u_ambientLight), 1);
  }
|gsl}

  class type shader = object
    method use : unit

    method set_ambient_light: float -> float -> float -> unit
    method set_light_position: float -> float -> float -> unit
    method set_object_matrix: Float32Array.t -> unit
    method set_world_matrix: Float32Array.t -> unit

    method set_positions: attrib_array -> unit
    method set_colors: attrib_array -> unit
    method set_normals: attrib_array -> unit

    method draw_arrays: mode -> ?first:int -> int -> unit
    method draw_elements: mode -> element_array -> unit
  end

  let init gl =
    let vertex_shader = new_shader gl vertex_shader `Vertex in
    let fragment_shader = new_shader gl fragment_shader `Fragment in
    let program = compile_program gl vertex_shader fragment_shader in
    let position_location = get_and_enable_vertex_attrib_array_location gl program "a_position" in
    let normal_location = get_and_enable_vertex_attrib_array_location gl program "a_normal" in
    let color_location = get_and_enable_vertex_attrib_array_location gl program "a_color" in
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
    (object
      method use = use_program gl program

      method set_ambient_light r g b =
        uniform3f gl ambient_light r g b
      method set_light_position x y z =
        uniform3f gl light_position x y z
      method set_object_matrix data =
        uniform_matrix4fv gl object_matrix false data
      method set_world_matrix data =
        uniform_matrix4fv gl world_matrix false data

      method set_positions a =
        a # plug position_location
      method set_colors a =
        a # plug color_location
      method set_normals a =
        a # plug normal_location

      method draw_arrays mode ?(first = 0) count =
        Webgl.draw_arrays gl (constant_of_mode mode) first count

      method draw_elements mode elements =
        elements # bind;
        Webgl.draw_elements gl (constant_of_mode mode) (elements # size) (elements # index_type) 0
    end : shader)

end

module Texture = struct
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

  class type shader = object
    method use : unit

    method set_world_matrix: Float32Array.t -> unit

    method binds_triangles: buffer -> unit
    method binds_texcoords: buffer -> unit
    method binds_texture: texture -> unit
  end

  let init gl =
    let vertex_shader = new_shader gl vertex_shader `Vertex in
    let fragment_shader = new_shader gl fragment_shader `Fragment in
    let program = compile_program gl vertex_shader fragment_shader in
    let position_location =
      get_and_enable_vertex_attrib_array_location gl program "a_position"
    in
    let world_matrix =
      match get_uniform_location gl program "u_matrix" with
      | Some thing -> thing
      | None -> error "unable to get 'u_matrix'"
    in
    let texcoord_location =
      get_and_enable_vertex_attrib_array_location gl program "a_texcoord"
    in
    let binds dim location buffer =
      bind_buffer gl _ARRAY_BUFFER_ buffer;
      vertex_attrib_pointer gl location dim _FLOAT_ false 0 0;
    in

    (object
      method use = use_program gl program

      method set_world_matrix data =
        uniform_matrix4fv gl world_matrix false data

      method binds_triangles buffer =
        binds 3 position_location buffer

      method binds_texcoords buffer =
        binds 2 texcoord_location buffer

      method binds_texture texture =
        bind_texture gl _TEXTURE_2D_ texture

    end : shader)

  class drawable gl shader =
    let texture = create_texture gl in
    let triangles = create_buffer gl in
    let texcoords = create_buffer gl in
    let fill_float_buffer buffer data =
        bind_buffer gl _ARRAY_BUFFER_ buffer;
        buffer_data gl _ARRAY_BUFFER_ (Float32Array.t_to_js data) _STATIC_DRAW_;
    in
    object
      val mutable size = 0

      method set_triangles data =
        size <- (Float32Array.length data) / 3;
        fill_float_buffer triangles data

      method set_texcoords data =
        fill_float_buffer texcoords data

      method set_texture canvas =
        bind_texture gl _TEXTURE_2D_ texture;
        tex_image_2D gl _TEXTURE_2D_ 0 _RGBA_ _RGBA_ _UNSIGNED_BYTE_  (`Canvas canvas);
        tex_parameteri gl _TEXTURE_2D_ _TEXTURE_MAG_FILTER_ _LINEAR_;
        tex_parameteri gl _TEXTURE_2D_ _TEXTURE_MIN_FILTER_ _LINEAR_MIPMAP_LINEAR_;
        generate_mipmap gl _TEXTURE_2D_

      method draw =
        enable gl _CULL_FACE_;
        enable gl _BLEND_;
        depth_mask gl false;
        blend_func gl _SRC_ALPHA_ _ONE_MINUS_SRC_ALPHA_;

        shader # binds_triangles triangles;
        shader # binds_texcoords texcoords;
        shader # binds_texture texture;

        draw_arrays gl _TRIANGLES_ 0 size;
        depth_mask gl true;
        disable gl _BLEND_;
        disable gl _CULL_FACE_

      initializer
        (* Starts with a white pixel (really load the texture later) *)
        bind_texture gl _TEXTURE_2D_ texture;
        let white = Uint8Array.new_uint8_array (`Data [| 255; 255; 255; 255|]) in
        tex_image_2D_array gl _TEXTURE_2D_ 0 _RGBA_ 1 1 0 _RGBA_ _UNSIGNED_BYTE_ (`Bytes white)
    end
end

module Basic2d = struct
  let vertex_shader = {gsl|
  attribute vec3 a_position;
  attribute vec3 a_color;

  uniform mat4 u_matrix;

  varying mediump vec3 v_color;

  void main() {
    v_color = a_color;
    gl_Position = u_matrix * vec4(a_position,1);
  }
|gsl}

  let fragment_shader = {gsl|
  precision mediump float;

  varying vec3 v_color;

  void main() {
    gl_FragColor = vec4(v_color, 1.0);
  }
|gsl}

  class type shader = object
    method use : unit

    method set_matrix: Float32Array.t -> unit

    method set_positions: attrib_array -> unit
    method set_colors: attrib_array -> unit

    method draw_arrays: mode -> ?first:int -> int -> unit
    method draw_elements: mode -> element_array -> unit
  end

  let init gl =
    let vertex_shader = new_shader gl vertex_shader `Vertex in
    let fragment_shader = new_shader gl fragment_shader `Fragment in
    let program = compile_program gl vertex_shader fragment_shader in
    let position_location = get_and_enable_vertex_attrib_array_location gl program "a_position" in
    let color_location = get_and_enable_vertex_attrib_array_location gl program "a_color" in
    let world_matrix =
      match get_uniform_location gl program "u_matrix" with
      | Some thing -> thing
      | None -> error "unable to get 'u_matrix'"
    in
    (object
      method use = use_program gl program

      method set_matrix data =
        uniform_matrix4fv gl world_matrix false data

      method set_positions a =
        a # plug position_location

      method set_colors a =
        a # plug color_location

      method draw_arrays mode ?(first = 0) count =
        Webgl.draw_arrays gl (constant_of_mode mode) first count

      method draw_elements mode elements =
        elements # bind;
        Webgl.draw_elements gl (constant_of_mode mode) (elements # size) (elements # index_type) 0
    end : shader)
end

