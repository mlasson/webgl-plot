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
    method name : string
    method draw : int -> int -> unit
    method opaque : bool
    method magnetize: float * float * float -> float * float * float
    method ray: three Vector.vector -> three Vector.vector -> three Vector.vector option

    method bounds: Webgl_plot_geometry.box
  end

class with_alpha ?alpha () =
  let alpha, opaque =
    match alpha with
    | Some alpha -> alpha, false
    | None -> 1.0, true
  in
  object
    val mutable alpha = alpha
    val mutable opaque = opaque
    method opaque = opaque
    method set_alpha = function
      | None ->
        begin
          alpha <- 1.0;
          opaque <- true;
        end
      | Some a ->
        begin
          alpha <- a;
          opaque <- false
        end
  end

class not_intersectable =
  object
    method ray (_ : three Vector.vector) (_ : three Vector.vector) = (None : three Vector.vector option)
    method magnetize (x : float * float * float) = x
  end
