(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

open Webgl
open Webgl.Constant
open Js_array
open Webgl_plot_misc

module Geometry = Webgl_plot_geometry

let error s = failwith s

let id_generator = ref 0

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

let get_vertex_attrib_array_location gl program location =
  let attrib_location = get_attrib_location gl program location in
  if attrib_location < 0 then
    error (Printf.sprintf "unable to get '%s'" location);
  attrib_location

let get_uniform_location gl program location =
  match get_uniform_location gl program location with
  | Some thing -> thing
  | None -> error (Printf.sprintf "unable to get '%s'" location)

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
    method fill (data : Index.t) =
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

class fbo gl init_xres init_yres =
  object(this)
    val framebuffer = create_framebuffer gl
    val texture = create_texture gl
    val depth_buffer = create_renderbuffer gl

    val mutable xres = 0
    val mutable yres = 0

    method resize xres' yres' =
      if xres' <> xres || yres' <> yres then begin
        xres <- xres';
        yres <- yres';
        bind_texture gl _TEXTURE_2D_ (Some texture);
        let colors_type =
          if get_extension gl "OES_texture_float" <> None then
            _FLOAT_
          else
            _UNSIGNED_BYTE_
        in
        tex_image_2D_array gl _TEXTURE_2D_ 0 _RGBA_ xres yres 0 _RGBA_ colors_type None;
        tex_parameteri gl _TEXTURE_2D_ _TEXTURE_MAG_FILTER_ _NEAREST_;
        tex_parameteri gl _TEXTURE_2D_ _TEXTURE_MIN_FILTER_ _NEAREST_;
        tex_parameteri gl _TEXTURE_2D_ _TEXTURE_WRAP_S_ _CLAMP_TO_EDGE_;
        tex_parameteri gl _TEXTURE_2D_ _TEXTURE_WRAP_T_ _CLAMP_TO_EDGE_;
        bind_framebuffer gl _FRAMEBUFFER_ (Some framebuffer);
        framebuffer_texture_2D gl _FRAMEBUFFER_ _COLOR_ATTACHMENT0_ _TEXTURE_2D_ texture 0;
        bind_renderbuffer gl _RENDERBUFFER_ depth_buffer;
        renderbuffer_storage gl _RENDERBUFFER_ _DEPTH_COMPONENT16_ xres yres;
        framebuffer_renderbuffer gl _FRAMEBUFFER_ _DEPTH_ATTACHMENT_ _RENDERBUFFER_ depth_buffer;
        bind_framebuffer gl _FRAMEBUFFER_ None;
        bind_texture gl _TEXTURE_2D_ None
      end

    method texture = texture

    method bind =
      bind_framebuffer gl _FRAMEBUFFER_ (Some framebuffer);
      viewport gl 0 0 xres yres

    initializer
      this # resize init_xres init_yres

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
  attribute vec3 a_shrink_direction;
  attribute vec3 a_color;

  uniform mat4 u_world_matrix;
  uniform mat4 u_object_matrix;

  uniform vec3 u_shrink;
  uniform vec3 u_explode;

  varying mediump vec3 v_position;
  varying mediump vec3 v_normal;
  varying mediump vec3 v_color;

  void main() {

    vec4 pos = u_world_matrix * u_object_matrix * vec4(a_position + u_shrink * a_shrink_direction + u_explode * a_normal,1);
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
  uniform float u_alpha;

  void main() {
    vec3 lightDirection = normalize(u_lightPos - v_position);
    float lighting = abs(dot(normalize(v_normal), lightDirection));
    gl_FragColor = vec4( v_color * (0.3 * lighting + 0.7 * u_ambientLight), u_alpha);
  }
|gsl}

  class type shader = object
    method use : unit
    method switch : unit
    method id : int

    method set_alpha: float -> unit
    method set_explode: float * float * float -> unit
    method set_shrink: float * float * float -> unit
    method set_ambient_light: float -> float -> float -> unit
    method set_light_position: float -> float -> float -> unit
    method set_object_matrix: Float32Array.t -> unit
    method set_world_matrix: Float32Array.t -> unit

    method set_positions: attrib_array -> unit
    method set_colors: attrib_array -> unit
    method set_normals: attrib_array -> unit
    method set_shrink_directions: attrib_array -> unit

    method draw_arrays: mode -> ?first:int -> int -> unit
    method draw_elements: mode -> element_array -> unit
  end

  let init gl =
    let vertex_shader = new_shader gl vertex_shader `Vertex in
    let fragment_shader = new_shader gl fragment_shader `Fragment in
    let program = compile_program gl vertex_shader fragment_shader in
    let position_location = get_vertex_attrib_array_location gl program "a_position" in
    let normal_location = get_vertex_attrib_array_location gl program "a_normal" in
    let color_location = get_vertex_attrib_array_location gl program "a_color" in
    let shrink_direction_location = get_vertex_attrib_array_location gl program "a_shrink_direction" in
    let world_matrix = get_uniform_location gl program "u_world_matrix" in
    let object_matrix = get_uniform_location gl program "u_object_matrix" in
    let ambient_light = get_uniform_location gl program "u_ambientLight" in
    let light_position = get_uniform_location gl program "u_lightPos" in
    let alpha = get_uniform_location gl program "u_alpha" in
    let shrink = get_uniform_location gl program "u_shrink" in
    let explode = get_uniform_location gl program "u_explode" in
    (object
      val id = !id_generator
      method id = id
      method use =
        use_program gl program;
        enable_vertex_attrib_array gl position_location;
        enable_vertex_attrib_array gl normal_location;
        enable_vertex_attrib_array gl color_location;
        enable_vertex_attrib_array gl shrink_direction_location

      method switch =
        disable_vertex_attrib_array gl position_location;
        disable_vertex_attrib_array gl normal_location;
        disable_vertex_attrib_array gl color_location;
        disable_vertex_attrib_array gl shrink_direction_location

      method set_ambient_light r g b =
        uniform3f gl ambient_light r g b
      method set_light_position x y z =
        uniform3f gl light_position x y z
      method set_alpha a =
        uniform1f gl alpha a
      method set_shrink (x,y,z) =
        uniform3f gl shrink x y z
      method set_explode (x,y,z) =
        uniform3f gl explode x y z
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
      method set_shrink_directions a =
        a # plug shrink_direction_location

      method draw_arrays mode ?(first = 0) count =
        Webgl.draw_arrays gl (constant_of_mode mode) first count

      method draw_elements mode elements =
        elements # bind;
        Webgl.draw_elements gl (constant_of_mode mode) (elements # size) (elements # index_type) 0

      initializer
        incr id_generator
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
    method switch : unit
    method id : int

    method set_world_matrix: Float32Array.t -> unit

    method binds_triangles: buffer -> unit
    method binds_texcoords: buffer -> unit
    method binds_texture: texture -> unit
  end

  let init gl =
    let vertex_shader = new_shader gl vertex_shader `Vertex in
    let fragment_shader = new_shader gl fragment_shader `Fragment in
    let program = compile_program gl vertex_shader fragment_shader in
    let position_location = get_vertex_attrib_array_location gl program "a_position" in
    let world_matrix = get_uniform_location gl program "u_matrix" in
    let texcoord_location = get_vertex_attrib_array_location gl program "a_texcoord" in

    let binds dim location buffer =
      bind_buffer gl _ARRAY_BUFFER_ buffer;
      vertex_attrib_pointer gl location dim _FLOAT_ false 0 0;
    in
    (object
      val id = !id_generator
      method id = id
      method use =
        use_program gl program;
        enable_vertex_attrib_array gl position_location;
        enable_vertex_attrib_array gl texcoord_location
      method switch =
        disable_vertex_attrib_array gl position_location;
        disable_vertex_attrib_array gl texcoord_location

      method set_world_matrix data =
        uniform_matrix4fv gl world_matrix false data

      method binds_triangles buffer =
        binds 3 position_location buffer

      method binds_texcoords buffer =
        binds 2 texcoord_location buffer

      method binds_texture texture =
        bind_texture gl _TEXTURE_2D_ (Some texture)

      initializer
        incr id_generator
    end : shader)

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
  uniform float u_alpha;
  void main() {
    gl_FragColor = vec4(v_color, u_alpha);
  }
|gsl}

  class type shader = object
    method use : unit
    method switch : unit
    method id : int

    method set_matrix: Float32Array.t -> unit

    method set_alpha: float -> unit
    method set_positions: attrib_array -> unit
    method set_colors: attrib_array -> unit

    method draw_arrays: mode -> ?first:int -> int -> unit
    method draw_elements: mode -> element_array -> unit
  end

  let init gl =
    let vertex_shader = new_shader gl vertex_shader `Vertex in
    let fragment_shader = new_shader gl fragment_shader `Fragment in
    let program = compile_program gl vertex_shader fragment_shader in
    let position_location = get_vertex_attrib_array_location gl program "a_position" in
    let color_location = get_vertex_attrib_array_location gl program "a_color" in
    let world_matrix = get_uniform_location gl program "u_matrix" in
    let alpha = get_uniform_location gl program "u_alpha" in
    (object
      val id = !id_generator
      method id = id
      method use =
        use_program gl program;
        enable_vertex_attrib_array gl position_location;
        enable_vertex_attrib_array gl color_location

      method switch =
        disable_vertex_attrib_array gl position_location;
        disable_vertex_attrib_array gl color_location

      method set_matrix data =
        uniform_matrix4fv gl world_matrix false data

      method set_alpha a =
        uniform1f gl alpha a

      method set_positions a =
        a # plug position_location

      method set_colors a =
        a # plug color_location

      method draw_arrays mode ?(first = 0) count =
        Webgl.draw_arrays gl (constant_of_mode mode) first count

      method draw_elements mode elements =
        elements # bind;
        Webgl.draw_elements gl (constant_of_mode mode) (elements # size) (elements # index_type) 0

      initializer
        incr id_generator
    end : shader)
end

module LightAndTexture = struct
  let vertex_shader = {gsl|
  attribute vec3 a_position;
  attribute vec3 a_normal;
  attribute vec2 a_texcoord;

  uniform mat4 u_world_matrix;
  uniform mat4 u_object_matrix;

  varying mediump vec3 v_position;
  varying mediump vec3 v_normal;
  varying mediump vec2 v_texcoord;

  void main() {

    vec4 pos = u_world_matrix * u_object_matrix * vec4(a_position,1);
    vec4 norm = u_world_matrix * u_object_matrix * vec4(a_normal,1);

    v_position = pos.xyz;
    v_normal = norm.xyz;
    v_texcoord = a_texcoord;

    gl_Position = pos;
  }
|gsl}

  let fragment_shader = {gsl|
  precision mediump float;

  varying vec3 v_position;
  varying vec3 v_normal;
  varying vec2 v_texcoord;

  uniform vec3 u_lightPos;
  uniform vec3 u_ambientLight;

  uniform sampler2D u_texture;

  void main() {
    vec3 lightDirection = normalize(u_lightPos - v_position);
    float lighting = abs(dot(normalize(v_normal), lightDirection));
    gl_FragColor = texture2D(u_texture, v_texcoord) * vec4((0.6 * lighting + 0.4 * u_ambientLight), 1.0);
  }
|gsl}

  class type shader = object
    method use : unit
    method switch : unit
    method id : int

    method set_ambient_light: float -> float -> float -> unit
    method set_light_position: float -> float -> float -> unit
    method set_object_matrix: Float32Array.t -> unit
    method set_world_matrix: Float32Array.t -> unit

    method set_positions: attrib_array -> unit
    method set_texcoords: attrib_array -> unit
    method set_normals: attrib_array -> unit

    method draw_arrays: mode -> ?first:int -> int -> unit
    method draw_elements: mode -> element_array -> unit
  end

  let init gl =
    let vertex_shader = new_shader gl vertex_shader `Vertex in
    let fragment_shader = new_shader gl fragment_shader `Fragment in
    let program = compile_program gl vertex_shader fragment_shader in
    let position_location = get_vertex_attrib_array_location gl program "a_position" in
    let normal_location = get_vertex_attrib_array_location gl program "a_normal" in
    let texcoord_location = get_vertex_attrib_array_location gl program "a_texcoord" in
    let world_matrix = get_uniform_location gl program "u_world_matrix" in
    let object_matrix = get_uniform_location gl program "u_object_matrix" in
    let ambient_light = get_uniform_location gl program "u_ambientLight" in
    let light_position = get_uniform_location gl program "u_lightPos" in
    (object
      val id = !id_generator
      method id = id
      method use =
        use_program gl program;
        enable_vertex_attrib_array gl position_location;
        enable_vertex_attrib_array gl normal_location;
        enable_vertex_attrib_array gl texcoord_location

      method switch =
        disable_vertex_attrib_array gl position_location;
        disable_vertex_attrib_array gl normal_location;
        disable_vertex_attrib_array gl texcoord_location

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
      method set_texcoords a =
        a # plug texcoord_location
      method set_normals a =
        a # plug normal_location

      method draw_arrays mode ?(first = 0) count =
        Webgl.draw_arrays gl (constant_of_mode mode) first count

      method draw_elements mode elements =
        elements # bind;
        Webgl.draw_elements gl (constant_of_mode mode) (elements # size) (elements # index_type) 0

      initializer
        incr id_generator
    end : shader)

end

module Screen = struct
  let vertex_shader = {gsl|
  attribute vec2 a_position;
  varying mediump vec2 v_position;

  void main() {
    v_position = a_position;
    gl_Position = vec4(a_position, 0, 1);
  }
|gsl}

  let fragment_shader = {gsl|
  precision mediump float;
  uniform sampler2D u_texture;

  varying vec2 v_position;

  void main() {
    gl_FragColor = min(vec4(1,1,1,1), texture2D(u_texture, 0.5 * (v_position + 1.0)));
  }
|gsl}

  class type shader = object
    method use : unit
    method switch : unit
    method id : int

    method draw: unit
  end

  let init gl =
    let vertex_shader = new_shader gl vertex_shader `Vertex in
    let fragment_shader = new_shader gl fragment_shader `Fragment in
    let program = compile_program gl vertex_shader fragment_shader in
    let position_location = get_vertex_attrib_array_location gl program "a_position" in
    let big_triangle =
       create_attrib_array gl 2 (float32_array [|
          -1.0; -1.0; -1.0; 4.0; 4.0; -1.0
       |]);
    in
    (object
      val id = !id_generator
      method id = id

      method use =
        use_program gl program;
        enable_vertex_attrib_array gl position_location;
        big_triangle # plug position_location

      method switch =
        disable_vertex_attrib_array gl position_location;

      method draw =
        Webgl.draw_arrays gl _TRIANGLES_ 0 3

      initializer
        incr id_generator
    end : shader)

end


