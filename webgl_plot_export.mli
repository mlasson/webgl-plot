type specification =
  | Constant of {
      data : float * float * float
    }
  | TimeConstant of {
      data : (float * float * float) array
    }
  | Pointwise of {
      data: (float * float * float) array array
    }
  | TimePointwise of {
      data: (float * float * float) array array array
    }
  | Unknown of Ojs.t [@js.default]
[@@js.sum]

type series_specification =
  | Uniform of {
      x: float array;
      z: float array;
      y: float array array;
    }

  | Parametric of {
      a: float array;
      b: float array;
      p: (float * float * float) array array;
    }

  | TimeUniform of {
      x: float array;
      z: float array;
      t: float array;
      y: float array array array;
    }

  | TimeParametric of {
      a: float array;
      b: float array;
      t: float array;
      p: (float * float * float) array array array;
    }
  | Unknown of Ojs.t [@js.default]
[@@js.sum]


type series =
  | Histogram of {
      name: string option;
      data: series_specification;
      colors: specification option;
      widths: specification option;
      wireframe: bool option;
    }

  | Scatter of {
      name: string option;
      data: series_specification;
      colors: specification option;
      widths: specification option;
      wireframe: bool option;
    }

  | Surface of {
      name: string option;
      data: series_specification;
      colors: specification;
      wireframe: bool option;
    }
  | Unknown of Ojs.t [@js.default]
[@@js.sum]

type pointer_kind =
  | Cross[@js "cross"]
  | Sphere[@js "sphere"]
  | None[@js "none"]
  | Unknown of Ojs.t [@js.default]
[@@js.sum]

type tick = {
  value: float;
  label: string;
}

type axis_option = {
  label: string option;
  ticks: tick list option;
}

type chart = {
  x_label: axis_option option;
  y_label: axis_option option;
  z_label: axis_option option;
  t_label: axis_option option;

  series: series list;

  pointer_kind: pointer_kind option;

  magnetic: bool option;
  morphing: bool option;
}
