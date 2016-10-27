(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

module Event : sig
  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val target: t -> Ojs.t
  val prevent_default: t -> unit
  val type_: t -> string

  val init_event: t -> string -> bool -> bool -> unit

  val client_x: t -> int (* mouse *)
  val client_y: t -> int (* mouse *)

  val screen_x: t -> int (* mouse *)
  val screen_y: t -> int (* mouse *)

  val buttons: t -> int  (* mouse *)

  val alt_key: t -> bool (* key *)
  val shift_key: t -> bool (* key *)

  val delta_y: t -> float (* wheel *)
end

module Rect : sig
  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val height: t -> float
  val width: t -> float
  val left: t -> float
  val right: t -> float
  val top: t -> float
  val bottom: t -> float
end

module Style : sig
  type t
  val set_color: t -> string -> unit
  val set_border: t -> string -> unit
  val set_background: t -> string -> unit
  val set_background_color: t -> string -> unit
  val set_height: t -> string -> unit
  val set_width: t -> string -> unit
  val set_bottom: t -> string -> unit
  val set_left: t -> string -> unit
  val set_top: t -> string -> unit
  val set_right: t -> string -> unit
  val set_cursor: t -> string -> unit
end


module Element : sig
  (* Only arguments marked with a "T" may be a textNode. *)
  (* When element arguments are required to be a specific element,
      it is marked with <tag> (where 'tag' is the element's tag name). *)

  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val null: t
  [@@js.custom
    let null = t_of_js Ojs.null
  ]

  val clone_node: t (* T *) -> bool -> t
  val contains: t (* T *) -> t (* T *) -> bool
  val append_child: t -> t (* T *) -> unit
  val insert_before: t -> t (* T *) -> t (* T *) -> unit
  val replace_child: t -> t (* T *) -> t (* T *) -> unit
  val remove_child: t -> t (* T *) -> unit
  val first_child: t -> t (* May return Element.null *)
  val last_child: t -> t (* May return Element.null *)
  val next_sibling: t (* T *) -> t (* May return Element.null *)
  val has_child_nodes: t (* T *) -> bool [@@js.call]
  val add_event_listener: t (* T *) -> string -> (Event.t -> unit) -> bool -> unit

  val has_attribute: t -> string -> bool
  val get_attribute: t -> string -> string
  val remove_attribute: t -> string -> unit
  val set_attribute: t -> string -> string -> unit
  val get_bounding_client_rect: t -> Rect.t [@@js.call]

  val normalize: t (* T *) -> unit

  val value: t (* <input> *) -> string
  val set_value: t (* <input> *) -> string -> unit

  val node_value: t (* T *) -> string
  val set_node_value: t (* T *) -> string -> unit
  val parent_node: t (* T *) -> t
  val node_name: t (* T *) -> string

  val dispatch_event: t (* T *) -> Event.t -> bool
  val style: t (* T *) -> Style.t
  val set_inner_HTML: t -> string -> unit
  val set_text_content: t -> string -> unit
  val set_class_name: t -> string -> unit
end

module Document: sig
  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val close: t -> unit
  val create_element: t -> string -> Element.t
  val create_text_node: t -> string -> Element.t
  val create_event: t -> string -> Event.t

  val get_element_by_id: t -> string -> Element.t option
  val get_elements_by_class_name: t -> string -> Element.t array

  val body: t -> Element.t
end

module Window: sig
  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  type timeout_id
  val timeout_id_of_js: Ojs.t -> timeout_id
  val timeout_id_to_js: timeout_id -> Ojs.t

  val document: t -> Document.t
  val set_onload: t -> (unit -> unit) -> unit
  val set_interval: t -> (unit -> unit) -> int -> timeout_id
  val set_timeout: t -> (unit -> unit) -> int -> timeout_id
  val clear_timeout: t -> timeout_id -> unit
  val request_animation_frame: t -> (float -> unit) -> unit

  val alert: t -> string -> unit
end

module JSON : sig
  val parse: string -> Ojs.t
    [@@js.global "JSON.parse"]
  val stringify: Ojs.t -> string
    [@@js.global "JSON.stringify"]
end

module File : sig
  type t = private Ojs.t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t
  val name: t -> string
end

module FileList : sig
  type t = private Ojs.t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t
  val item: t -> int -> File.t
  val length: t -> int
  val files:  (* <input> *) Element.t -> t
end

module FileReader : sig
  type state =
    | Empty [@js 0]
    | Loading [@js 1]
    | Done [@js 2] [@@js.enum]

  type t = private Ojs.t

  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t
  val new_file_reader: unit -> t [@@js.new]
  val ready_state: t -> state
  val result: t -> string
  val set_onload: t -> (unit -> unit) -> unit
  val read_as_text: t -> File.t -> unit
end

module XHR: sig
  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t


  val create: unit -> t [@@js.new "XMLHttpRequest"]
  val open_: t -> string -> string -> unit
  val send: t -> string -> unit
  val set_request_header: t -> string -> string -> unit
  val override_mime_type: t -> string -> unit
  val set_with_credentials: t -> bool -> unit (* starting from IE10 *)

  type ready_state =
    | Unsent [@js 0]
    | Opened [@js 1]
    | Headers_received [@js 2]
    | Loading [@js 3]
    | Done [@js 4]
    | Other of int [@js.default]
    [@@js.enum]

  val status: t -> int
  val ready_state: t -> ready_state
  val response_text: t -> string

  val set_onreadystatechange: t -> (unit -> unit) -> unit
end

val window: Window.t
val document: Document.t

module Console: sig
    val log: Ojs.t -> unit [@@js.global]
end [@js.scope "console"]

module Canvas : sig
  type context
  val context_of_js: Ojs.t -> context
  val context_to_js: context -> Ojs.t

  type gradient
  val gradient_of_js: Ojs.t -> gradient
  val gradient_to_js: gradient -> Ojs.t

  type css_color = string

  type context_attribute = { alpha: bool; }

  val get_context:?alpha:bool -> (*<canvas>*) Element.t -> context option
  [@@js.custom
      val get_context_internal: Element.t -> string -> context_attribute -> context option
         [@@js.call "getContext"]
      let get_context ?(alpha = true) canvas =
         get_context_internal canvas "2d" {alpha}
  ]

  val set_fill_style: context -> ([`Color of css_color | `Gradient of gradient][@js.union]) -> unit
  val set_stroke_style: context -> ([`Color of css_color | `Gradient of gradient][@js.union]) -> unit
  val set_line_width: context -> float -> unit
  val create_linear_gradient: context -> float -> float -> float -> float -> gradient
  val add_color_stop: gradient -> float -> css_color -> unit
  val begin_path: context -> unit
  val close_path: context -> unit
  val move_to: context -> float -> float -> unit
  val line_to: context -> float -> float -> unit
  val fill: context -> unit
  val stroke: context -> unit
  val stroke_rect: context -> float -> float -> float -> float -> unit
  val fill_rect: context -> float -> float -> float -> float -> unit
  val set_font: context -> string -> unit
  val fill_text: context -> string -> float -> float -> unit
  val stroke_text: context -> string -> float -> float -> unit
  module TextMetrics : sig
    type t
    val t_of_js: Ojs.t -> t
    val t_to_js: t -> Ojs.t
    val width: t -> float
  end
  val measure_text: context -> string -> TextMetrics.t
  val rotate: context -> float -> unit
  val translate: context -> float -> float -> unit
  val scale: context -> float -> float -> unit
  val clear_rect: context -> float -> float -> float -> float -> unit
end
