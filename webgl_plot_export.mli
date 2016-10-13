module Histogram : sig
  type t =
    | Uniform of {
        name: string option;

        x: float array;
        z: float array;
        y: float array array;

        widths: float array array option;
        colors: float array array array option;
        wireframe: bool option;
      } [@js "uniform"]

    | Parametric of {
        name: string option;

        a: float array;
        b: float array;
        p: float array array array;

        widths: float array array option;
        colors: float array array array option;
        wireframe: bool option;
      } [@js "parametric"]

    | Unknown of Ojs.t [@js.default]
  [@@js.sum "representation"]
end

module Surface : sig
  type t =
    | Uniform of {
        name: string option;

        x: float array;
        z: float array;
        y: float array array;

        colors: float array array array option;
        alpha: float option;
        wireframe: bool option;
      }[@js "uniform"]

    | Parametric of {
        name: string option;

        a: float array;
        b: float array;
        p: float array array array;

        colors: float array array array option;
        alpha: float option;
        wireframe: bool option;
      }[@js "parametric"]

    | Unknown of Ojs.t [@js.default]
  [@@js.sum "representation"]

end

type series =
  | Histogram of Histogram.t [@js.arg "data"][@js "histogram"]
  | Scatter of Histogram.t [@js.arg "data"][@js "scatter"]
  | Surface of Surface.t [@js.arg "data"][@js "surface"]
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
  bounds: (float * float) option;
}

type chart = {
  x_axis: axis_option option;
  y_axis: axis_option option;
  z_axis: axis_option option;

  series: series list;

  pointer_kind: pointer_kind option;

  magnetic: bool option;

  ratio: (float * float * float) option;
}
val chart_to_js: chart -> Ojs.t
val chart_of_js: Ojs.t -> chart
