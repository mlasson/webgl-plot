open Js_bindings

module Examples = struct

  let surface scene =
    let n = 100 in
    let m = n in
    let xs = Geometry.uniform_array n 0.0 1.0 in
    let zs = Geometry.uniform_array m 0.0 1.0 in
    let ys = Array.create_float (n * m) in
    let alpha = 3.0 in
    let beta = 6.0 in
    begin
      for i = 0 to n - 1 do
        for j = 0 to m - 1 do
          ys.(i * m + j) <-
            (Math.sq (cos (alpha *. xs.(i))) +. Math.sq (sin (beta *. zs.(j)))) *. 0.5
        done
      done
    end;
    scene # add_surface xs zs ys

  let histogram scene =
    let n = 10 in
    let m = n in
    let xs = Geometry.uniform_array n 0.0 1.0 in
    let zs = Geometry.uniform_array m 0.0 1.0 in
    let ys = Array.create_float ((n- 1) * (m-1)) in
    let alpha = 3.0 in
    let beta = 6.0 in
    begin
      for i = 0 to n - 2 do
        for j = 0 to m - 2 do
          ys.(i * (m - 1) + j) <-
            (Math.sq (cos (alpha *. xs.(i))) +. Math.sq (sin (beta *. zs.(j)))) *. 0.5
        done
      done
    end;
    scene # add_histogram xs zs ys

end


let () = Window.set_onload window (fun _ ->
  let main = Helper.element_of_id "main" in
  let renderer gl =
    let open Scene in
    let scene = prepare_scene gl in
    let repere = scene # repere in
    repere # set_x_axis_label "XXX axis label";
    repere # set_y_axis_label "YYY axis label";
    repere # set_z_axis_label "ZZZ axis label";
    repere # set_frame {x_max = 1.;x_min=0.;y_max=1.;y_min=0.;z_max=1.;z_min=0.};
    Examples.surface scene;
    Examples.histogram scene;
    fun _clock {Component.aspect; angle; move; pointer; _} ->
      scene # set_aspect aspect;
      scene # set_angle angle;
      scene # set_move move;
      scene # set_pointer pointer;
      scene # render
  in
  let canvas = Component.create_webgl_canvas renderer in
  Element.append_child main canvas)
