(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

module Kinds = struct
  type unknown [@@js]
  module Node = struct
    type element [@@js]
    type text [@@js]
    type comment [@@js]
    type processing_instruction_node [@@js]
    type document [@@js]
    type document_type [@@js]
    type document_fragment [@@js]
    type deprecated [@@js]
  end

  module Html = struct
    type body [@@js]
    type input [@@js]
    type canvas [@@js]

    type table [@@js]
    type tbody [@@js]
    type td [@@js]
    type th [@@js]
    type thead [@@js]
    type tr [@@js]
  end
end

open Kinds

module Node : sig
  type 'a t = private Ojs.t
  val t_of_js: (Ojs.t -> 'a) -> Ojs.t -> 'a t
  val t_to_js: ('a -> Ojs.t) -> 'a t -> Ojs.t

  (** Node API *)

  val append_child: 'a t -> 'b t -> unit
  val base_URI: 'a t -> string
  val clone_node: 'a t -> 'a t
  val first_child: 'a t -> unknown t option
  val has_child_nodes: 'a t -> bool
  val last_child: 'a t -> unknown t option
  val remove_child: 'a t -> 'b t -> unit

  val set_text_content: 'a t -> string -> unit
  val get_text_content: 'a t -> string -> unit

  open Kinds.Node
  val node_type: 'a t ->
     [ `Element of element t
     | `Text of text t
     | `ProcessingInstructionNde of processing_instruction_node t
     | `Comment of comment t
     | `Document of document t
     | `DocumentType of document_type t
     | `DocumentFragment of document_fragment t
     | `Deprecated of deprecated t ]
end = struct
  include ([%js] : sig
    type untyped = private Ojs.t
    val untyped_of_js: Ojs.t -> untyped
    val untyped_to_js: untyped -> Ojs.t

    val append_child: untyped -> untyped -> unit
    val base_URI: untyped -> string
    val clone_node: untyped -> untyped
    val first_child: untyped -> untyped option
    val has_child_nodes: untyped -> bool
    val last_child: untyped -> untyped option
    val node_type: untyped -> int
    val remove_child: untyped -> untyped -> unit

    val set_text_content: untyped -> string -> unit
    val get_text_content: untyped -> string -> unit
  end)
  type 'a t = untyped
  let t_of_js _ x = untyped_of_js x
  let t_to_js _ x = untyped_to_js x

  let node_type x =
    let open Kinds.Node in
    match node_type x with
    | 1 -> `Element (x : element t)
    | 3 -> `Text (x : text t)
    | 7 -> `ProcessingInstructionNde (x : processing_instruction_node t)
    | 8 -> `Comment (x : comment t)
    | 9 -> `Document (x : document t)
    | 10 -> `DocumentType (x : document_type t)
    | 11 -> `DocumentFragment (x: document_fragment t)
    | _ -> `Deprecated (x: deprecated t)
end

module Element : sig
  type 'a t = Kinds.Node.element Node.t
  val t_of_js: (Ojs.t -> 'a) -> Ojs.t -> 'a t
  val t_to_js: ('a -> Ojs.t) -> 'a t -> Ojs.t

  val has_attribute: 'a t -> string -> bool
  val set_attribute: 'a t -> string -> string -> unit
  val get_attribute: 'a t -> string -> string
  val remove_attribute: 'a t -> string -> unit

  val set_class_name: 'a t -> string -> unit
  val get_class_name: 'a t -> string

  val set_innerHTML:  'a t -> string -> unit
  val inner_HTML: 'a t -> string

  val set_outer_HTML: 'a t -> string -> unit
  val outer_HTML: 'a t -> string

  val set_onclick: 'a t -> (unit -> unit) -> unit

  val unsafe_cast: 'a t -> 'b t
  val tag_name: 'a t -> string
end = struct
  include ([%js] :
    sig
      type untyped = Kinds.Node.element Node.t
      val untyped_of_js: Ojs.t -> untyped
      val untyped_to_js: untyped -> Ojs.t

      val has_attribute: untyped -> string -> bool
      val set_attribute: untyped -> string -> string -> unit
      val get_attribute: untyped -> string -> string
      val remove_attribute: untyped -> string -> unit

      val set_class_name: untyped -> string -> unit
      val get_class_name: untyped -> string

      val set_innerHTML:  untyped -> string -> unit
      val inner_HTML: untyped -> string

      val set_outer_HTML: untyped -> string -> unit
      val outer_HTML: untyped -> string

      val set_onclick: untyped -> (unit -> unit) -> unit
      val tag_name: untyped -> string
    end)
  type 'a t = untyped
  let t_of_js _ x = untyped_of_js x
  let t_to_js _ x = untyped_to_js x
  let unsafe_cast x = x
end

module Document = struct
  type t = Kinds.Node.document Node.t [@@js]

  include ([%js] : sig
    val set_title: t -> string -> unit
    val title: t -> string

    val get_element_by_id: t -> string -> unknown Element.t option
    val get_elements_by_class_name: t -> string -> unknown Element.t array
    val get_elements_by_name: t -> string -> unknown Element.t array

    val create_element: t -> string -> unknown Element.t
    val create_text_node: t -> string -> Kinds.Node.text Node.t

   val body: t -> Kinds.Html.body Element.t
  end)

  let create_html_input document =
    (create_element document "input"
    |> Element.unsafe_cast : Kinds.Html.input Element.t)
  let create_html_canvas document =
    (create_element document "canvas"
    |> Element.unsafe_cast : Kinds.Html.canvas Element.t)
  let create_html_table document =
    (create_element document "table"
    |> Element.unsafe_cast : Kinds.Html.table Element.t)
  let create_html_tr document =
    (create_element document "tr"
    |> Element.unsafe_cast : Kinds.Html.tr Element.t)
  let create_html_td document =
    (create_element document "td"
    |> Element.unsafe_cast : Kinds.Html.td Element.t)
  let create_html_th document =
    (create_element document "th"
    |> Element.unsafe_cast : Kinds.Html.th Element.t)
  let create_html_tbody document =
    (create_element document "tbody"
    |> Element.unsafe_cast : Kinds.Html.tbody Element.t)
  let create_html_thead document =
    (create_element document "thead"
    |> Element.unsafe_cast : Kinds.Html.thead Element.t)
end

module Window : sig
  type t = private Ojs.t

  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val document: t -> Document.t
  val request_animation_frame: t -> (float -> unit) -> unit

  val set_onload: t -> (unit -> unit) -> unit
end = [%js]

val window: Window.t
  [@@js]

val alert: string -> unit
  [@@js.global]

module Console : sig
  val log: Ojs.t -> unit
end[@js.scope "console"] = [%js] 

include ([%js] : sig
  val set_interval: (unit -> unit) -> float -> unit
  val set_timeout: (unit -> unit) -> float -> unit
end)

module JSON : sig
  val parse: string -> Ojs.t
    [@@js.global "JSON.parse"]
  val stringify: Ojs.t -> string
    [@@js.global "JSON.stringify"]
end = [%js]

module File : sig
  type t = private Ojs.t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t
  val name: t -> string
end = [%js]

module FileList : sig
  type t = private Ojs.t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t
  val item: t -> int -> File.t option
  val length: t -> int
end = [%js]

module FileReader = struct
  type state =
    | Empty [@js 0]
    | Loading [@js 1]
    | Done [@js 2] [@@js] [@@js.enum]

  include ([%js] : sig
    type t = private Ojs.t
    val t_of_js: Ojs.t -> t
    val t_to_js: t -> Ojs.t
    val new_file_reader : unit -> t [@@js.new]
    val ready_state : t -> state
    val result: t -> string option
    val set_onload: t -> (unit -> unit) -> unit
    val read_as_text: t -> File.t -> unit
  end)
end

module Float32Array = struct
   type t = private Ojs.t[@@js]
   include ([%js] : sig
     val new_float32_array: float array -> t [@@js.new]
   end)
end 

module Html = struct
  module Input = struct
     type t = Kinds.Html.input Element.t [@@js]
     include ([%js] : sig
       val files: t -> FileList.t
     end)
  end

  module Canvas = struct
    type t = Kinds.Html.canvas Element.t [@@js]
     
    module WebGl = struct
      type t = private Ojs.t [@@js] 
      type fragment[@@js]
      type vertex[@@js]
      type element[@@js]
      type array_buffer[@@js]

      include (struct
        include ([%js] : sig 
          type shader_parameter = private Ojs.t
          val shader_parameter_of_js: Ojs.t -> shader_parameter
          val shader_parameter_to_js: shader_parameter -> Ojs.t

          type untyped_shader = private Ojs.t
          val untyped_shader_of_js: Ojs.t -> untyped_shader
          val untyped_shader_to_js: untyped_shader -> Ojs.t

          type untyped_shader_type = private Ojs.t
          val untyped_shader_type_of_js: Ojs.t -> untyped_shader_type
          val untyped_shader_type_to_js: untyped_shader_type -> Ojs.t

          val vertex_shader_type: t -> untyped_shader_type[@@js.get "VERTEX_SHADER"] 
          val fragment_shader_type: t -> untyped_shader_type [@@js.get "FRAGMENT_SHADER"]

          val shader_delete_status: t -> shader_parameter [@@js.get "DELETE_STATUS"]
          val shader_compile_status: t -> shader_parameter [@@js.get "COMPILE_STATUS"]
          val shader_type: t -> shader_parameter [@@js.get "SHADER_TYPE"]

          val get_shader_parameter: t -> untyped_shader -> shader_parameter -> Ojs.t
          val create_shader: t -> untyped_shader_type -> untyped_shader
          val shader_source: t -> untyped_shader -> string -> unit
          val compile_shader: t -> untyped_shader -> unit
          val get_shader_info_log: t -> untyped_shader -> string
          
          type program = private Ojs.t
          val program_of_js: Ojs.t -> program
          val program_to_js: program -> Ojs.t
          val create_program: t -> program[@@js.call]
          val attach_shader: t -> program -> untyped_shader -> unit
          val link_program: t -> program -> unit
          val get_program_info_log: t -> program -> string
          val use_program: t -> program -> unit

          type untyped_program_parameter = private Ojs.t
          val untyped_program_parameter_of_js: Ojs.t -> untyped_program_parameter
          val untyped_program_parameter_to_js: untyped_program_parameter -> Ojs.t

          val program_delete_status: t -> untyped_program_parameter [@@js.get "DELETE_STATUS"]
          val program_link_status: t -> untyped_program_parameter [@@js.get "LINK_STATUS"]
          val program_validate_status: t -> untyped_program_parameter [@@js.get "VALIDATE_STATUS"]
          val program_attached_shaders: t -> untyped_program_parameter [@@js.get "ATTACHED_SHADERS"]
          val program_active_attibutes: t -> untyped_program_parameter [@@js.get "ACTIVE_ATTRIBUTES"]
          val program_active_uniforms: t -> untyped_program_parameter [@@js.get "ACTIVE_UNIFORMS"]
 
          val get_program_parameter: t -> program -> untyped_program_parameter -> Ojs.t
          val get_attrib_location: t -> program -> string -> int
          type uniform_location = private Ojs.t
          val uniform_location_of_js: Ojs.t -> uniform_location
          val uniform_location_to_js: uniform_location -> Ojs.t
          val get_uniform_location: t -> program -> string -> uniform_location option

          type untyped_buffer_kind = private Ojs.t
          val untyped_buffer_kind_of_js: Ojs.t -> untyped_buffer_kind
          val untyped_buffer_kind_to_js: untyped_buffer_kind -> Ojs.t
          val array_buffer: t -> untyped_buffer_kind [@@js.get "ARRAY_BUFFER"]
          val element_array_buffer: t -> untyped_buffer_kind [@@js.get"ELEMENT_ARRAY_BUFFER"]

          type usage = private Ojs.t
          val usage_of_js: Ojs.t -> usage
          val usage_to_js: usage -> Ojs.t
          val static_draw: t -> usage [@@js.get "STATIC_DRAW"]
          val dynamic_draw: t -> usage [@@js.get "DYNAMIC_DRAW"]
          val stream_draw: t -> usage [@@js.get "STREAM_DRAW"]
         
          type untyped_buffer = private Ojs.t
          val untyped_buffer_of_js: Ojs.t -> untyped_buffer
          val untyped_buffer_to_js: untyped_buffer -> Ojs.t
          val create_buffer: t -> untyped_buffer[@@js.call]

          type untyped_data = private Ojs.t
          val untyped_data_of_js: Ojs.t -> untyped_data
          val untyped_data_to_js: untyped_data -> Ojs.t
          
          val bind_buffer: t -> untyped_buffer_kind -> untyped_buffer -> unit
          val buffer_data: t -> untyped_buffer_kind -> Ojs.t -> usage -> unit
 
          type data_type = private Ojs.t
          val data_type_of_js: Ojs.t -> data_type
          val data_type_to_js: data_type -> Ojs.t
          val type_float: t -> data_type[@@js.get "FLOAT"]
          val enable_vertex_attrib_array: t -> int -> unit
          val vertex_attrib_pointer: t -> int -> int -> data_type -> bool -> int -> int -> unit 

          type mask = int
          val mask_of_js: Ojs.t -> mask
          val mask_to_js: mask -> Ojs.t
          val _COLOR_BUFFER_BIT_: t -> mask[@@js.get "COLOR_BUFFER_BIT"]
          val _DEPTH_BUFFER_BIT_: t -> mask[@@js.get "DEPTH_BUFFER_BIT"]
          val clear: t -> mask -> unit
          

          type mode = private Ojs.t
          val mode_of_js: Ojs.t -> mode
          val mode_to_js: mode -> Ojs.t
          
          val _TRIANGLES_: t -> mode [@@js.get "TRIANGLES"]
          val _POINTS_: t -> mode [@@js.get "POINTS"]
          val _TRIANGLE_STRIP_: t -> mode [@@js.get "TRIANGLE_STRIP"]
          val _LINE_STRIP_: t -> mode [@@js.get "LINE_STRIP"]
          val _LINES_:t -> mode [@@js.get "LINES"]

          
          val draw_arrays: t -> mode -> int -> int -> unit
          
          val uniform4f: t -> uniform_location -> float -> float -> float -> float -> unit
          val uniform3f: t -> uniform_location -> float -> float -> float -> unit
          val uniform_matrix4fv: t -> uniform_location -> bool -> Float32Array.t -> unit

          type capability = private Ojs.t
          val capability_of_js : Ojs.t -> capability
          val capability_to_js : capability -> Ojs.t

          val _DEPTH_TEST_: t -> capability [@@js.get "DEPTH_TEST"]
          val enable: t -> capability -> unit

          type func = private Ojs.t
          val func_of_js: Ojs.t -> func
          val func_to_js: func -> Ojs.t

          val _LESS_: t -> func
          val depth_func: t -> func -> unit 
 
        end)
 
        type 'a shader = untyped_shader 
        let shader_of_js _ = untyped_shader_of_js
        let shader_to_js _ = untyped_shader_to_js

        type 'a shader_type = untyped_shader_type
        let shader_type_of_js _ = untyped_shader_type_of_js
        let shader_type_to_js _ = untyped_shader_type_to_js

        let retype_shader gl shader = 
          let x = get_shader_parameter gl shader (shader_type gl) in
          if untyped_shader_type_of_js x = vertex_shader_type gl then
            `Vertex (shader : vertex shader)
          else if untyped_shader_type_of_js x = fragment_shader_type gl then
            `Fragment (shader : fragment shader)
          else assert false 

        let get_shader_parameter gl shader parameter = 
          Ojs.bool_of_js (get_shader_parameter gl shader parameter) 

        type 'a program_parameter = untyped_program_parameter

        let program_parameter_of_js _ = untyped_program_parameter_of_js 
        let program_parameter_to_js _ = untyped_program_parameter_to_js
        
        let get_program_int_parameter gl shader x = 
          Ojs.int_of_js (get_program_parameter gl shader x)
        let get_program_bool_parameter gl shader x = 
          Ojs.bool_of_js (get_program_parameter gl shader x)

        type 'a buffer = untyped_buffer 
        let buffer_of_js _ = untyped_buffer_of_js
        let buffer_to_js _ = untyped_buffer_to_js

        type 'a data = untyped_data 
        let data_of_js _ = untyped_data_of_js
        let data_to_js _ = untyped_data_to_js

        type 'a buffer_kind = untyped_buffer_kind 
        let buffer_kind_of_js _ = untyped_buffer_kind_of_js
        let buffer_kind_to_js _ = untyped_buffer_kind_to_js
 
        let buffer_data_size gl kind data usage = 
          buffer_data gl kind (Ojs.int_to_js data) usage

        let buffer_array_data gl kind data usage =
          buffer_data gl kind (Float32Array.t_to_js data) usage
         
       end : sig
         type shader_parameter = private Ojs.t
         val shader_parameter_of_js: Ojs.t -> shader_parameter
         val shader_parameter_to_js: shader_parameter -> Ojs.t
 
         type 'a shader = private Ojs.t
         val shader_of_js: (Ojs.t -> 'a) -> Ojs.t -> 'a shader
         val shader_to_js: ('a -> Ojs.t) -> 'a shader -> Ojs.t

         type 'a shader_type = private Ojs.t
         val shader_type_of_js: (Ojs.t -> 'a) -> Ojs.t -> 'a shader_type
         val shader_type_to_js: ('a -> Ojs.t) -> 'a shader_type -> Ojs.t

         val vertex_shader_type: t -> vertex shader_type
         val fragment_shader_type: t -> fragment shader_type

         val shader_delete_status: t -> shader_parameter
         val shader_compile_status: t -> shader_parameter

         val create_shader: t -> 'a shader_type -> 'a shader
         val shader_source: t -> 'a shader -> string -> unit
         val compile_shader: t -> 'a shader -> unit
         val retype_shader: t -> 'a shader -> [`Fragment of fragment shader | `Vertex of vertex shader]
         val get_shader_parameter: t -> 'a shader -> shader_parameter -> bool
         val get_shader_info_log: t -> 'a shader -> string

         type program = private Ojs.t
         val program_of_js: Ojs.t -> program
         val program_to_js: program -> Ojs.t
         val create_program: t -> program
         val attach_shader: t -> program -> 'a shader -> unit
         val link_program: t -> program -> unit
         val get_program_info_log: t -> program -> string
         val use_program: t -> program -> unit

         type 'a program_parameter
         val program_parameter_of_js: (Ojs.t -> 'a) -> Ojs.t -> 'a program_parameter
         val program_parameter_to_js: ('a -> Ojs.t) -> 'a program_parameter -> Ojs.t
         val program_delete_status: t -> bool program_parameter
         val program_link_status: t -> bool program_parameter
         val program_validate_status: t -> bool program_parameter
         val program_attached_shaders: t -> int program_parameter
         val program_active_attibutes: t -> int program_parameter
         val program_active_uniforms: t -> int program_parameter 

         val get_program_int_parameter: t -> program -> int program_parameter -> int
         val get_program_bool_parameter: t -> program -> bool program_parameter -> bool
         val get_attrib_location: t -> program -> string -> int

         type uniform_location = private Ojs.t
         val uniform_location_of_js: Ojs.t -> uniform_location
         val uniform_location_to_js: uniform_location -> Ojs.t
         val get_uniform_location: t -> program -> string -> uniform_location option

         type 'a buffer_kind = private Ojs.t
         val buffer_kind_of_js: (Ojs.t -> 'a) -> Ojs.t -> 'a buffer_kind
         val buffer_kind_to_js: ('a -> Ojs.t) -> 'a buffer_kind -> Ojs.t
         val array_buffer: t -> float buffer_kind
         val element_array_buffer: t -> Float32Array.t buffer_kind

         type usage = private Ojs.t
         val usage_of_js: Ojs.t -> usage
         val usage_to_js: usage -> Ojs.t
         val static_draw: t -> usage
         val dynamic_draw: t -> usage
         val stream_draw: t -> usage

         type 'a buffer = private Ojs.t
         val buffer_of_js: (Ojs.t -> 'a) -> Ojs.t -> 'a buffer
         val buffer_to_js: ('a -> Ojs.t) -> 'a buffer -> Ojs.t
         val create_buffer: t -> 'a buffer

         type 'a data = private Ojs.t
         val data_of_js: (Ojs.t -> 'a) -> Ojs.t -> 'a data
         val data_to_js: ('a -> Ojs.t) -> 'a data -> Ojs.t
          
         val bind_buffer: t -> 'a buffer_kind -> 'a buffer -> unit
         val buffer_data_size: t -> 'a buffer_kind -> int -> usage -> unit
         val buffer_data: t -> 'a buffer_kind -> Float32Array.t -> usage -> unit

         type data_type = private Ojs.t
         val data_type_of_js: Ojs.t -> data_type
         val data_type_to_js: data_type -> Ojs.t
         val type_float: t -> data_type
         val enable_vertex_attrib_array: t -> int -> unit
         val vertex_attrib_pointer: t -> int -> int -> data_type -> bool -> int -> int -> unit 

         type mask = int
         val mask_of_js: Ojs.t -> mask
         val mask_to_js: mask -> Ojs.t
         val _COLOR_BUFFER_BIT_: t -> mask
         val _DEPTH_BUFFER_BIT_: t -> mask
         val clear: t -> mask -> unit

         type mode = private Ojs.t
         val mode_of_js: Ojs.t -> mode
         val mode_to_js: mode -> Ojs.t
         val _TRIANGLES_: t -> mode
         val _POINTS_: t -> mode
         val _TRIANGLE_STRIP_: t -> mode
         val _LINE_STRIP_: t -> mode
         val _LINES_: t -> mode
         val draw_arrays: t -> mode -> int -> int -> unit

         val uniform4f: t -> uniform_location -> float -> float -> float -> float -> unit
         val uniform3f: t -> uniform_location -> float -> float -> float -> unit
         val uniform_matrix4fv: t -> uniform_location -> bool -> Float32Array.t -> unit
         
         type capability = private Ojs.t
         val capability_of_js : Ojs.t -> capability
         val capability_to_js : capability -> Ojs.t

         val _DEPTH_TEST_: t -> capability
         val enable: t -> capability -> unit

         type func = private Ojs.t
         val func_of_js: Ojs.t -> func
         val func_to_js: func -> Ojs.t

         val _LESS_: t -> func

         val depth_func: t -> func -> unit 
       end)

     end

     type context_type =
       | WebGl [@js "webgl"]
       [@@js] [@@js.enum]

     type context_attribute = {
       alpha: bool option;
       depth: bool option;
       stencil: bool option;
       antialias: bool option;
     } [@@js]

     let default_context_attribute = { 
       alpha = None;
       depth = None;
       stencil = None;
       antialias = None;
     }

     include ([%js] : sig
       val get_context: t -> context_type -> context_attribute -> WebGl.t option
     end)
     let get_context canvas ?(context_attribute = default_context_attribute) context_type = 
       get_context canvas context_type context_attribute
  end

  let retype x =
    match String.lowercase_ascii (Element.tag_name x) with
    | "input" -> `Input (Element.unsafe_cast x : Kinds.Html.input Element.t)
    | "canvas" -> `Canvas (Element.unsafe_cast x : Kinds.Html.canvas Element.t)
    | "table" -> `Table (Element.unsafe_cast x : Kinds.Html.table Element.t)
    | "tr" -> `Tr (Element.unsafe_cast x : Kinds.Html.tr Element.t)
    | "td" -> `Td (Element.unsafe_cast x : Kinds.Html.td Element.t)
    | "body" -> `Body (Element.unsafe_cast x : Kinds.Html.body Element.t)
    | "tbody" -> `Tbody (Element.unsafe_cast x : Kinds.Html.tbody Element.t)
    | "thead" -> `Thead (Element.unsafe_cast x : Kinds.Html.thead Element.t)
    | _ -> `Unknown
end
