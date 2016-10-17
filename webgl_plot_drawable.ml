open Webgl_plot_math

class type context =
  object
    method pointer : float * float
    method x_min: float
    method x_max: float
    method y_min: float
    method y_max: float
    method z_min: float
    method z_max: float
  end

class type drawable =
  object
    method draw : context -> int -> int -> unit
    method opaque : bool
    method ray: three Vector.vector -> three Vector.vector -> three Vector.vector option
  end

class dummy_ray =
  object
    method ray (_ : three Vector.vector) (_ : three Vector.vector) = (None : three Vector.vector option)
  end


