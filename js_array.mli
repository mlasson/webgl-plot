module Float32Array : sig
  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val new_float32_array: ([`Data of float array | `Size of int | `Copy of t][@js.union]) -> t [@@js.new]
  val length: t -> int

  val get : t -> int -> float
  [@@js.custom
    external get : t -> int -> float = "caml_js_get" ]
  val set : t -> int -> float -> unit
  [@@js.custom
    external set : t -> int -> float -> unit = "caml_js_set" ]
end

module Uint8Array : sig
  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val new_uint8_array: ([`Data of int array | `Size of int | `Copy of t][@js.union]) -> t [@@js.new]
  val length: t -> int

  val get : t -> int -> int
  [@@js.custom
    external get : t -> int -> int = "caml_js_get" ]
  val set : t -> int -> int -> unit
  [@@js.custom
    external set : t -> int -> int -> unit = "caml_js_set" ]
end

module Uint16Array : sig
  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val new_uint16_array: ([`Data of int array | `Size of int | `Copy of t][@js.union]) -> t [@@js.new]
  val length: t -> int

  val get : t -> int -> int
  [@@js.custom
    external get : t -> int -> int = "caml_js_get" ]
  val set : t -> int -> int -> unit
  [@@js.custom
    external set : t -> int -> int -> unit = "caml_js_set" ]
end

module Uint32Array : sig
  type t
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val new_uint32_array: ([`Data of int array | `Size of int | `Copy of t][@js.union]) -> t [@@js.new]
  val length: t -> int

  val get : t -> int -> int
  [@@js.custom
    external get : t -> int -> int = "caml_js_get" ]
  val set : t -> int -> int -> unit
  [@@js.custom
    external set : t -> int -> int -> unit = "caml_js_set" ]
end
