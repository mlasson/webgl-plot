open Js_bindings
open Plot

let () = Window.set_onload window (fun _ ->
  let open Models in
  let open Helper in
  let progress_bars = progress_bars () in
  let main = element_of_id "main" in
  let input = Document.create_html_input document in
  let panel = create "div" in
  let resolution = integer_input input in

  Node.append_child main input;
  Node.append_child main panel;

  let x_axis_label = "X axis label" in
  let y_axis_label = "Y axis label" in
  let z_axis_label = "Z axis label" in

  let open Math in
  let open Asynchronous_computations in
  resolution # on_change (function None -> () | Some res ->
    removeAll panel;
    Node.append_child panel (progress_bars # element);
    let thread =
      let context = (progress_bars :> context) in
      Surface.from_grid_fun ~context res (-2.0 *. pi) (2.0 *. pi) (-2.0 *. pi) (2.0 *. pi) (fun x y -> cos x +. sin y )
      >>= new_plot {height = 1080; width = 1920; x_axis_label; y_axis_label; z_axis_label} ~on_click:(fun (x,y,z) -> alert (Printf.sprintf "%f, %f, %f" x y z))
      >>= fun plot ->
        Node.remove_child panel (progress_bars # element);
        Node.append_child panel plot; return ()
    in
    ignore thread))

