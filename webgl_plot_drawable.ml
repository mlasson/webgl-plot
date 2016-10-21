(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

open Js_array
open Webgl_plot_math

class identified =
  let next_id = ref 0 in
  object
    val id = !next_id
    method id = id
    initializer
      incr next_id
  end

class type object3d =
  object
    method id : int
    method name : string
    method draw : int -> int -> unit
    method opaque : bool
    method magnetize: float * float * float -> float * float * float
    method ray: three Vector.vector -> three Vector.vector -> three Vector.vector option
    method x_projection: float -> (Float32Array.t * Float32Array.t) option
    method z_projection: float -> (Float32Array.t * Float32Array.t) option
  end

class no_projections =
  object
    method x_projection (_ : float) : (Float32Array.t * Float32Array.t) option = None
    method z_projection (_ : float) : (Float32Array.t * Float32Array.t) option = None
  end

class not_intersectable =
  object
    inherit no_projections
    method ray (_ : three Vector.vector) (_ : three Vector.vector) = (None : three Vector.vector option)
    method magnetize (x : float * float * float) = x
  end
