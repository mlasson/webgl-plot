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

class attrib_array gl dim data =
  let buffer = create_buffer gl in
  let size = Float32Array.length data in
  let count = assert (size mod dim = 0); size / dim in
  object(this)
    method count = count
    method dim = dim
    method bind =
      bind_buffer gl _ARRAY_BUFFER_ buffer
    method attach location =
      this # bind;
      vertex_attrib_pointer gl location dim _FLOAT_ false 0 0
    initializer
      this # bind;
      buffer_data gl _ARRAY_BUFFER_ (Float32Array.t_to_js data) _STATIC_DRAW_
  end

class element_array gl data =
  let buffer = create_buffer gl in
  let data, index_type, size =
    match data with
    | `Byte data -> Uint8Array.t_to_js data, _UNSIGNED_BYTE_, Uint8Array.length data
    | `Short data -> Uint16Array.t_to_js data, _UNSIGNED_SHORT_, Uint16Array.length data
    | `Int data -> Uint32Array.t_to_js data, _UNSIGNED_INT_, Uint32Array.length data
  in
  object(this)
    method index_type = index_type
    method size = size
    method buffer = buffer
    method bind =
      bind_buffer gl _ELEMENT_ARRAY_BUFFER_ buffer;
    initializer
      this # bind;
      buffer_data gl _ELEMENT_ARRAY_BUFFER_ data _STATIC_DRAW_
  end



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
    gl_FragColor = vec4( v_color * (0.0 * lighting + u_ambientLight), 1);
  }
|gsl}

  class type shader = object
    method use : unit

    method set_ambient_light: float -> float -> float -> unit
    method set_light_position: float -> float -> float -> unit
    method set_object_matrix: Float32Array.t -> unit
    method set_world_matrix: Float32Array.t -> unit

    method binds_positions: buffer -> unit
    method binds_colors: buffer -> unit
    method binds_normals: buffer -> unit
    method binds_indexes: buffer -> unit
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
    let binds location buffer =
      bind_buffer gl _ARRAY_BUFFER_ buffer;
      vertex_attrib_pointer gl location 3 _FLOAT_ false 0 0
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

      method binds_positions buffer =
        binds position_location buffer

      method binds_colors buffer =
        binds color_location buffer

      method binds_normals buffer =
        binds normal_location buffer

      method binds_indexes buffer =
        bind_buffer gl _ELEMENT_ARRAY_BUFFER_ buffer
    end : shader)

  class drawable gl shader =

    let positions = Webgl.create_buffer gl in
    let colors = Webgl.create_buffer gl in
    let normals = Webgl.create_buffer gl in
    let indexes = Webgl.create_buffer gl in

    let fill_float_buffer buffer data =
        bind_buffer gl _ARRAY_BUFFER_ buffer;
        buffer_data gl _ARRAY_BUFFER_ (Float32Array.t_to_js data) _STATIC_DRAW_;
    in

    object(this)
      val mutable size = 0
      val mutable index_type = _UNSIGNED_SHORT_
      val mutable cull_face = false
      val mutable mode = _TRIANGLES_

      method set_cull_face b = cull_face <- b
      method set_mode m = mode <- m

      method set_positions data = fill_float_buffer positions data
      method set_normals data = fill_float_buffer normals data
      method set_colors data = fill_float_buffer colors data

      method set_indexes8 data =
        size <- Uint8Array.length data;
        index_type <- _UNSIGNED_BYTE_;
        bind_buffer gl _ELEMENT_ARRAY_BUFFER_ indexes;
        buffer_data gl _ELEMENT_ARRAY_BUFFER_ (Uint8Array.t_to_js data) _STATIC_DRAW_

      method set_indexes16 data =
        size <- Uint16Array.length data;
        index_type <- _UNSIGNED_SHORT_;
        bind_buffer gl _ELEMENT_ARRAY_BUFFER_ indexes;
        buffer_data gl _ELEMENT_ARRAY_BUFFER_ (Uint16Array.t_to_js data) _STATIC_DRAW_

      method set_indexes32 data =
        index_type <- _UNSIGNED_INT_;
        size <- Uint32Array.length data;
        bind_buffer gl _ELEMENT_ARRAY_BUFFER_ indexes;
        buffer_data gl _ELEMENT_ARRAY_BUFFER_ (Uint32Array.t_to_js data) _STATIC_DRAW_

      method set_indexes (data : Geometry.indexes) =
        match data with
        | `Byte data -> this # set_indexes8 data
        | `Short data -> this # set_indexes16 data
        | `Int data -> this # set_indexes32 data

      method draw =
        (if cull_face then enable gl _CULL_FACE_ else disable gl _CULL_FACE_);
        shader # binds_positions positions;
        shader # binds_normals normals;
        shader # binds_colors colors;
        shader # binds_indexes indexes;
        draw_elements gl mode size index_type 0
    end
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
        tex_parameteri gl _TEXTURE_2D_ _TEXTURE_MIN_FILTER_ _LINEAR_

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
