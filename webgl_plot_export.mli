(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

module Histogram : sig
  type t =
    | Uniform of {
        name: string option;

        x: float array;
        z: float array;
        y: float array array;

        border: float option;

        widths: float array array option;
        depths: float array array option;
        colors: float array array array option;
      } [@js "uniform"]

    | List of {
        name: string option;

        border: float option;

        centers: float array array;
        widths: float array option;
        depths: float array option;
        colors: float array array option;
      } [@js "list"]

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
        magnetic: bool option;

      }[@js "uniform"]

    | Parametric of {
        name: string option;

        a: float array;
        b: float array;
        p: float array array array;

        colors: float array array array option;
        alpha: float option;
        wireframe: bool option;
        magnetic: bool option;
      }[@js "parametric"]

    | Unknown of Ojs.t [@js.default]
  [@@js.sum "representation"]

end

type series =
  | Histogram of Histogram.t [@js.arg "data"][@js "histogram"]
  | Surface of Surface.t [@js.arg "data"][@js "surface"]
  | Unknown of Ojs.t [@js.default]
[@@js.sum]

type tick = {
  value: float;
  label: string;
}
val tick_to_js: tick -> Ojs.t
val tick_of_js: Ojs.t -> tick

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

  ratio: (float * float * float) option;
}

val chart_to_js: chart -> Ojs.t
val chart_of_js: Ojs.t -> chart
