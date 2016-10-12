[@@@comment "!! This code has been generated by gen_js_api !!"]
[@@@ocaml.warning "-7-32-39"]
module Histogram =
  struct
    type t =
      | Uniform of
      {
      name: string option ;
      x: float array ;
      z: float array ;
      y: float array array ;
      widths: float array array option ;
      colors: float array array array option ;
      wireframe: bool option } 
      | Parametric of
      {
      name: string option ;
      a: float array ;
      b: float array ;
      p: float array array array ;
      widths: float array array option ;
      colors: float array array array option ;
      wireframe: bool option } 
      | Unknown of Ojs.t 
    let rec (t_of_js : Ojs.t -> t) =
      fun x32  ->
        let x33 = x32  in
        match Ojs.type_of (Ojs.get x33 "representation") with
        | "number" ->
            (match Ojs.int_of_js (Ojs.get x33 "representation") with
             | _ -> Unknown x33)
        | "string" ->
            (match Ojs.string_of_js (Ojs.get x33 "representation") with
             | "uniform" ->
                 Uniform
                   {
                     name =
                       (Ojs.option_of_js Ojs.string_of_js
                          (Ojs.get x33 "name"));
                     x = (Ojs.array_of_js Ojs.float_of_js (Ojs.get x33 "x"));
                     z = (Ojs.array_of_js Ojs.float_of_js (Ojs.get x33 "z"));
                     y =
                       (Ojs.array_of_js
                          (fun x37  -> Ojs.array_of_js Ojs.float_of_js x37)
                          (Ojs.get x33 "y"));
                     widths =
                       (Ojs.option_of_js
                          (fun x39  ->
                             Ojs.array_of_js
                               (fun x40  ->
                                  Ojs.array_of_js Ojs.float_of_js x40) x39)
                          (Ojs.get x33 "widths"));
                     colors =
                       (Ojs.option_of_js
                          (fun x42  ->
                             Ojs.array_of_js
                               (fun x43  ->
                                  Ojs.array_of_js
                                    (fun x44  ->
                                       Ojs.array_of_js Ojs.float_of_js x44)
                                    x43) x42) (Ojs.get x33 "colors"));
                     wireframe =
                       (Ojs.option_of_js Ojs.bool_of_js
                          (Ojs.get x33 "wireframe"))
                   }
             | "parametric" ->
                 Parametric
                   {
                     name =
                       (Ojs.option_of_js Ojs.string_of_js
                          (Ojs.get x33 "name"));
                     a = (Ojs.array_of_js Ojs.float_of_js (Ojs.get x33 "a"));
                     b = (Ojs.array_of_js Ojs.float_of_js (Ojs.get x33 "b"));
                     p =
                       (Ojs.array_of_js
                          (fun x50  ->
                             Ojs.array_of_js
                               (fun x51  ->
                                  Ojs.array_of_js Ojs.float_of_js x51) x50)
                          (Ojs.get x33 "p"));
                     widths =
                       (Ojs.option_of_js
                          (fun x53  ->
                             Ojs.array_of_js
                               (fun x54  ->
                                  Ojs.array_of_js Ojs.float_of_js x54) x53)
                          (Ojs.get x33 "widths"));
                     colors =
                       (Ojs.option_of_js
                          (fun x56  ->
                             Ojs.array_of_js
                               (fun x57  ->
                                  Ojs.array_of_js
                                    (fun x58  ->
                                       Ojs.array_of_js Ojs.float_of_js x58)
                                    x57) x56) (Ojs.get x33 "colors"));
                     wireframe =
                       (Ojs.option_of_js Ojs.bool_of_js
                          (Ojs.get x33 "wireframe"))
                   }
             | _ -> Unknown x33)
        | _ -> Unknown x33
    
    and (t_to_js : t -> Ojs.t) =
      fun x1  ->
        match x1 with
        | Uniform x2 ->
            Ojs.obj
              [|("representation", (Ojs.string_to_js "uniform"));("name",
                                                                   (Ojs.option_to_js
                                                                    Ojs.string_to_js
                                                                    x2.name));
                ("x", (Ojs.array_to_js Ojs.float_to_js x2.x));("z",
                                                                (Ojs.array_to_js
                                                                   Ojs.float_to_js
                                                                   x2.z));
                ("y",
                  (Ojs.array_to_js
                     (fun x6  -> Ojs.array_to_js Ojs.float_to_js x6) 
                     x2.y));("widths",
                              (Ojs.option_to_js
                                 (fun x8  ->
                                    Ojs.array_to_js
                                      (fun x9  ->
                                         Ojs.array_to_js Ojs.float_to_js x9)
                                      x8) x2.widths));("colors",
                                                        (Ojs.option_to_js
                                                           (fun x11  ->
                                                              Ojs.array_to_js
                                                                (fun x12  ->
                                                                   Ojs.array_to_js
                                                                    (fun x13 
                                                                    ->
                                                                    Ojs.array_to_js
                                                                    Ojs.float_to_js
                                                                    x13) x12)
                                                                x11)
                                                           x2.colors));
                ("wireframe", (Ojs.option_to_js Ojs.bool_to_js x2.wireframe))|]
        | Parametric x16 ->
            Ojs.obj
              [|("representation", (Ojs.string_to_js "parametric"));("name",
                                                                    (Ojs.option_to_js
                                                                    Ojs.string_to_js
                                                                    x16.name));
                ("a", (Ojs.array_to_js Ojs.float_to_js x16.a));("b",
                                                                 (Ojs.array_to_js
                                                                    Ojs.float_to_js
                                                                    x16.b));
                ("p",
                  (Ojs.array_to_js
                     (fun x20  ->
                        Ojs.array_to_js
                          (fun x21  -> Ojs.array_to_js Ojs.float_to_js x21)
                          x20) x16.p));("widths",
                                         (Ojs.option_to_js
                                            (fun x23  ->
                                               Ojs.array_to_js
                                                 (fun x24  ->
                                                    Ojs.array_to_js
                                                      Ojs.float_to_js x24)
                                                 x23) x16.widths));("colors",
                                                                    (Ojs.option_to_js
                                                                    (fun x26 
                                                                    ->
                                                                    Ojs.array_to_js
                                                                    (fun x27 
                                                                    ->
                                                                    Ojs.array_to_js
                                                                    (fun x28 
                                                                    ->
                                                                    Ojs.array_to_js
                                                                    Ojs.float_to_js
                                                                    x28) x27)
                                                                    x26)
                                                                    x16.colors));
                ("wireframe",
                  (Ojs.option_to_js Ojs.bool_to_js x16.wireframe))|]
        | Unknown x31 ->
            Ojs.obj
              [|("representation", (Ojs.string_to_js "Unknown"));("arg", x31)|]
    
  end
module Surface =
  struct
    type t =
      | Uniform of
      {
      name: string option ;
      x: float array ;
      z: float array ;
      y: float array array ;
      colors: float array array array option ;
      wireframe: bool option } 
      | Parametric of
      {
      name: string option ;
      a: float array ;
      b: float array ;
      p: float array array array ;
      colors: float array array array option ;
      wireframe: bool option } 
      | Unknown of Ojs.t 
    let rec (t_of_js : Ojs.t -> t) =
      fun x86  ->
        let x87 = x86  in
        match Ojs.type_of (Ojs.get x87 "representation") with
        | "number" ->
            (match Ojs.int_of_js (Ojs.get x87 "representation") with
             | _ -> Unknown x87)
        | "string" ->
            (match Ojs.string_of_js (Ojs.get x87 "representation") with
             | "uniform" ->
                 Uniform
                   {
                     name =
                       (Ojs.option_of_js Ojs.string_of_js
                          (Ojs.get x87 "name"));
                     x = (Ojs.array_of_js Ojs.float_of_js (Ojs.get x87 "x"));
                     z = (Ojs.array_of_js Ojs.float_of_js (Ojs.get x87 "z"));
                     y =
                       (Ojs.array_of_js
                          (fun x91  -> Ojs.array_of_js Ojs.float_of_js x91)
                          (Ojs.get x87 "y"));
                     colors =
                       (Ojs.option_of_js
                          (fun x93  ->
                             Ojs.array_of_js
                               (fun x94  ->
                                  Ojs.array_of_js
                                    (fun x95  ->
                                       Ojs.array_of_js Ojs.float_of_js x95)
                                    x94) x93) (Ojs.get x87 "colors"));
                     wireframe =
                       (Ojs.option_of_js Ojs.bool_of_js
                          (Ojs.get x87 "wireframe"))
                   }
             | "parametric" ->
                 Parametric
                   {
                     name =
                       (Ojs.option_of_js Ojs.string_of_js
                          (Ojs.get x87 "name"));
                     a = (Ojs.array_of_js Ojs.float_of_js (Ojs.get x87 "a"));
                     b = (Ojs.array_of_js Ojs.float_of_js (Ojs.get x87 "b"));
                     p =
                       (Ojs.array_of_js
                          (fun x101  ->
                             Ojs.array_of_js
                               (fun x102  ->
                                  Ojs.array_of_js Ojs.float_of_js x102) x101)
                          (Ojs.get x87 "p"));
                     colors =
                       (Ojs.option_of_js
                          (fun x104  ->
                             Ojs.array_of_js
                               (fun x105  ->
                                  Ojs.array_of_js
                                    (fun x106  ->
                                       Ojs.array_of_js Ojs.float_of_js x106)
                                    x105) x104) (Ojs.get x87 "colors"));
                     wireframe =
                       (Ojs.option_of_js Ojs.bool_of_js
                          (Ojs.get x87 "wireframe"))
                   }
             | _ -> Unknown x87)
        | _ -> Unknown x87
    
    and (t_to_js : t -> Ojs.t) =
      fun x61  ->
        match x61 with
        | Uniform x62 ->
            Ojs.obj
              [|("representation", (Ojs.string_to_js "uniform"));("name",
                                                                   (Ojs.option_to_js
                                                                    Ojs.string_to_js
                                                                    x62.name));
                ("x", (Ojs.array_to_js Ojs.float_to_js x62.x));("z",
                                                                 (Ojs.array_to_js
                                                                    Ojs.float_to_js
                                                                    x62.z));
                ("y",
                  (Ojs.array_to_js
                     (fun x66  -> Ojs.array_to_js Ojs.float_to_js x66) 
                     x62.y));("colors",
                               (Ojs.option_to_js
                                  (fun x68  ->
                                     Ojs.array_to_js
                                       (fun x69  ->
                                          Ojs.array_to_js
                                            (fun x70  ->
                                               Ojs.array_to_js
                                                 Ojs.float_to_js x70) x69)
                                       x68) x62.colors));("wireframe",
                                                           (Ojs.option_to_js
                                                              Ojs.bool_to_js
                                                              x62.wireframe))|]
        | Parametric x73 ->
            Ojs.obj
              [|("representation", (Ojs.string_to_js "parametric"));("name",
                                                                    (Ojs.option_to_js
                                                                    Ojs.string_to_js
                                                                    x73.name));
                ("a", (Ojs.array_to_js Ojs.float_to_js x73.a));("b",
                                                                 (Ojs.array_to_js
                                                                    Ojs.float_to_js
                                                                    x73.b));
                ("p",
                  (Ojs.array_to_js
                     (fun x77  ->
                        Ojs.array_to_js
                          (fun x78  -> Ojs.array_to_js Ojs.float_to_js x78)
                          x77) x73.p));("colors",
                                         (Ojs.option_to_js
                                            (fun x80  ->
                                               Ojs.array_to_js
                                                 (fun x81  ->
                                                    Ojs.array_to_js
                                                      (fun x82  ->
                                                         Ojs.array_to_js
                                                           Ojs.float_to_js
                                                           x82) x81) x80)
                                            x73.colors));("wireframe",
                                                           (Ojs.option_to_js
                                                              Ojs.bool_to_js
                                                              x73.wireframe))|]
        | Unknown x85 ->
            Ojs.obj
              [|("representation", (Ojs.string_to_js "Unknown"));("arg", x85)|]
    
  end
type series =
  | Histogram of Histogram.t 
  | Scatter of Histogram.t 
  | Surface of Surface.t 
  | Unknown of Ojs.t 
let rec (series_of_js : Ojs.t -> series) =
  fun x114  ->
    let x115 = x114  in
    match Ojs.type_of (Ojs.get x115 "kind") with
    | "number" ->
        (match Ojs.int_of_js (Ojs.get x115 "kind") with | _ -> Unknown x115)
    | "string" ->
        (match Ojs.string_of_js (Ojs.get x115 "kind") with
         | "histogram" -> Histogram (Histogram.t_of_js (Ojs.get x115 "data"))
         | "scatter" -> Scatter (Histogram.t_of_js (Ojs.get x115 "data"))
         | "surface" -> Surface (Surface.t_of_js (Ojs.get x115 "data"))
         | _ -> Unknown x115)
    | _ -> Unknown x115

and (series_to_js : series -> Ojs.t) =
  fun x109  ->
    match x109 with
    | Histogram x110 ->
        Ojs.obj
          [|("kind", (Ojs.string_to_js "histogram"));("data",
                                                       (Histogram.t_to_js
                                                          x110))|]
    | Scatter x111 ->
        Ojs.obj
          [|("kind", (Ojs.string_to_js "scatter"));("data",
                                                     (Histogram.t_to_js x111))|]
    | Surface x112 ->
        Ojs.obj
          [|("kind", (Ojs.string_to_js "surface"));("data",
                                                     (Surface.t_to_js x112))|]
    | Unknown x113 ->
        Ojs.obj [|("kind", (Ojs.string_to_js "Unknown"));("arg", x113)|]

type pointer_kind =
  | Cross 
  | Sphere 
  | None 
  | Unknown of Ojs.t 
let rec (pointer_kind_of_js : Ojs.t -> pointer_kind) =
  fun x118  ->
    let x119 = x118  in
    match Ojs.type_of (Ojs.get x119 "kind") with
    | "number" ->
        (match Ojs.int_of_js (Ojs.get x119 "kind") with | _ -> Unknown x119)
    | "string" ->
        (match Ojs.string_of_js (Ojs.get x119 "kind") with
         | "cross" -> Cross
         | "sphere" -> Sphere
         | "none" -> None
         | _ -> Unknown x119)
    | _ -> Unknown x119

and (pointer_kind_to_js : pointer_kind -> Ojs.t) =
  fun x116  ->
    match x116 with
    | Cross  -> Ojs.obj [|("kind", (Ojs.string_to_js "cross"))|]
    | Sphere  -> Ojs.obj [|("kind", (Ojs.string_to_js "sphere"))|]
    | None  -> Ojs.obj [|("kind", (Ojs.string_to_js "none"))|]
    | Unknown x117 ->
        Ojs.obj [|("kind", (Ojs.string_to_js "Unknown"));("arg", x117)|]

type tick = {
  value: float ;
  label: string }
let rec (tick_of_js : Ojs.t -> tick) =
  fun x121  ->
    {
      value = (Ojs.float_of_js (Ojs.get x121 "value"));
      label = (Ojs.string_of_js (Ojs.get x121 "label"))
    }

and (tick_to_js : tick -> Ojs.t) =
  fun x120  ->
    Ojs.obj
      [|("value", (Ojs.float_to_js x120.value));("label",
                                                  (Ojs.string_to_js
                                                     x120.label))|]

type axis_option =
  {
  label: string option ;
  ticks: tick list option ;
  bounds: (float* float) option }
let rec (axis_option_of_js : Ojs.t -> axis_option) =
  fun x130  ->
    {
      label = (Ojs.option_of_js Ojs.string_of_js (Ojs.get x130 "label"));
      ticks =
        (Ojs.option_of_js (fun x132  -> Ojs.list_of_js tick_of_js x132)
           (Ojs.get x130 "ticks"));
      bounds =
        (Ojs.option_of_js
           (fun x134  ->
              let x135 = x134  in
              ((Ojs.float_of_js (Ojs.array_get x135 0)),
                (Ojs.float_of_js (Ojs.array_get x135 1))))
           (Ojs.get x130 "bounds"))
    }

and (axis_option_to_js : axis_option -> Ojs.t) =
  fun x122  ->
    Ojs.obj
      [|("label", (Ojs.option_to_js Ojs.string_to_js x122.label));("ticks",
                                                                    (
                                                                    Ojs.option_to_js
                                                                    (fun x124
                                                                     ->
                                                                    Ojs.list_to_js
                                                                    tick_to_js
                                                                    x124)
                                                                    x122.ticks));
        ("bounds",
          (Ojs.option_to_js
             (fun x126  ->
                let (x127,x128) = x126  in
                let x129 = Ojs.array_make 2  in
                Ojs.array_set x129 0 (Ojs.float_to_js x127);
                Ojs.array_set x129 1 (Ojs.float_to_js x128);
                x129) x122.bounds))|]

type chart =
  {
  x_axis: axis_option option ;
  y_axis: axis_option option ;
  z_axis: axis_option option ;
  series: series list ;
  pointer_kind: pointer_kind option ;
  magnetic: bool option ;
  ratio: (float* float* float) option }
let rec (chart_of_js : Ojs.t -> chart) =
  fun x148  ->
    {
      x_axis = (Ojs.option_of_js axis_option_of_js (Ojs.get x148 "xAxis"));
      y_axis = (Ojs.option_of_js axis_option_of_js (Ojs.get x148 "yAxis"));
      z_axis = (Ojs.option_of_js axis_option_of_js (Ojs.get x148 "zAxis"));
      series = (Ojs.list_of_js series_of_js (Ojs.get x148 "series"));
      pointer_kind =
        (Ojs.option_of_js pointer_kind_of_js (Ojs.get x148 "pointerKind"));
      magnetic = (Ojs.option_of_js Ojs.bool_of_js (Ojs.get x148 "magnetic"));
      ratio =
        (Ojs.option_of_js
           (fun x155  ->
              let x156 = x155  in
              ((Ojs.float_of_js (Ojs.array_get x156 0)),
                (Ojs.float_of_js (Ojs.array_get x156 1)),
                (Ojs.float_of_js (Ojs.array_get x156 2))))
           (Ojs.get x148 "ratio"))
    }

and (chart_to_js : chart -> Ojs.t) =
  fun x136  ->
    Ojs.obj
      [|("xAxis", (Ojs.option_to_js axis_option_to_js x136.x_axis));("yAxis",
                                                                    (Ojs.option_to_js
                                                                    axis_option_to_js
                                                                    x136.y_axis));
        ("zAxis", (Ojs.option_to_js axis_option_to_js x136.z_axis));("series",
                                                                    (Ojs.list_to_js
                                                                    series_to_js
                                                                    x136.series));
        ("pointerKind",
          (Ojs.option_to_js pointer_kind_to_js x136.pointer_kind));("magnetic",
                                                                    (Ojs.option_to_js
                                                                    Ojs.bool_to_js
                                                                    x136.magnetic));
        ("ratio",
          (Ojs.option_to_js
             (fun x143  ->
                let (x144,x145,x146) = x143  in
                let x147 = Ojs.array_make 3  in
                Ojs.array_set x147 0 (Ojs.float_to_js x144);
                Ojs.array_set x147 1 (Ojs.float_to_js x145);
                Ojs.array_set x147 2 (Ojs.float_to_js x146);
                x147) x136.ratio))|]

let (chart_to_js : chart -> Ojs.t) = chart_to_js 
let (chart_of_js : Ojs.t -> chart) = chart_of_js 
