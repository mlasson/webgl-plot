open Js_bindings

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
    let n = 10 in
    let m = n in
    let xs = Geometry.uniform_array n 0.0 1.0 in
    let zs = Geometry.uniform_array m 0.0 1.0 in
    let ys = Array.create_float (n * m) in
    let alpha = 3.0 in
    let beta = 6.0 in
    begin
      for i = 0 to n - 1 do
        for j = 0 to m - 1 do
          ys.(i * m + j) <- (Math.sq (cos (alpha *. xs.(i))) +. Math.sq (sin (beta *. zs.(j)))) *. 0.5
        done
      done
    end;
    scene # add_surface xs zs ys;
    let last_add = ref 0.0 in
    fun clock {Component.aspect; angle; move; _} ->
      if clock -. !last_add  > 10.0 then begin
        last_add := clock;
        let position = Random.float 1.0, Random.float 1.0, 0.5 in
        let color = Random.float 1.0, Random.float 1.0, Random.float 1.0 in
        let scale =
          let s = Random.float 0.01 in
          s, s, s
        in
        scene # add_sphere position scale color;
      end;
      scene # set_aspect aspect;
      scene # set_angle angle;
      scene # set_move move;
      scene # render
  in
  let canvas = Component.create_webgl_canvas renderer in
  Element.append_child main canvas)
