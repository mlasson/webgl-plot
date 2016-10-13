open Webgl_plot_math

class type context =
  object
    method pointer : float * float
  end

class type drawable =
  object
    method draw : context -> int -> unit
    method ray: three Vector.vector -> three Vector.vector -> three Vector.vector option
  end

class dummy_ray =
  object
    method ray (_ : three Vector.vector) (_ : three Vector.vector) = (None : three Vector.vector option)
  end


