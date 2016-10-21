(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

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
    method draw : int -> int -> unit
    method opaque : bool
    method magnetize: float * float * float -> float * float * float
    method ray: three Vector.vector -> three Vector.vector -> three Vector.vector option
  end

class not_intersectable =
  object
    method ray (_ : three Vector.vector) (_ : three Vector.vector) = (None : three Vector.vector option)
    method magnetize (x : float * float * float) = x
  end
