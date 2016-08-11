open Js_bindings

let () = Window.set_onload window (fun _ ->
  let open Drawing in
  let open Helper in
  let progress_bars = progress_bars () in
  let main = element_of_id "main" in
  let input = Document.create_html_input document in
  let panel = create "div" in
  let resolution = integer_input input in

  Node.append_child main input;
  Node.append_child main panel;

  let open Math in
  let open Computations in
  resolution # on_change (function None -> () | Some res ->
    removeAll panel;
    Node.append_child panel (progress_bars # element);
    let thread =
      let context = (progress_bars :> context) in
      Surface.from_fun ~context res (-. pi) pi (-. pi) pi (fun x y -> sin (sqrt (x *. x +. y *. y)))
      >>= new_plot {height = 800; width = 800}
      >>= fun plot ->
        Node.remove_child panel (progress_bars # element);
        Node.append_child panel plot; return ()
    in
    ignore thread))

