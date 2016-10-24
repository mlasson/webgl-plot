(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

(** Serializable description of plots. *)

(** Description of histograms. *)
module Histogram : sig
  type t =
    | Grid of {
        name: string option;

        x: float array;
        z: float array;
        y: float array array;

        border: float option;

        widths: float array array option;
        depths: float array array option;
        colors: (float * float * float) array array option;
      } [@js "grid"]

    | List of {
        name: string option;

        border: float option;

        centers: (float * float * float) array;
        widths: float array option;
        depths: float array option;
        colors: (float * float * float) array option;
      } [@js "list"]

    | Unknown of Ojs.t [@js.default]
  [@@js.sum "representation"]

  (**/**)
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  (**/**)

end

(** Description of surfaces. *)
module Surface : sig
  type t =
    | Graph of {
        name: string option;

        x: float array;
        z: float array;
        y: float array array;

        colors: (float * float * float) array array option;
        alpha: float option;
        wireframe: bool option;
        magnetic: bool option;

      }[@js "graph"]

    | Parametric of {
        name: string option;

        a: float array;
        b: float array;
        p: (float * float * float) array array;

        colors: (float * float * float) array array option;
        alpha: float option;
        wireframe: bool option;
        magnetic: bool option;
      }[@js "parametric"]

    | Unknown of Ojs.t [@js.default]
  [@@js.sum "representation"]

  (**/**)
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  (**/**)
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

(**/**)
val chart_to_js: chart -> Ojs.t
val chart_of_js: Ojs.t -> chart
val tick_to_js: tick -> Ojs.t
val tick_of_js: Ojs.t -> tick
(**/**)
