open Js_bindings
open Models
open Html
open Canvas.WebGl
open Helper
open Asynchronous_computations

let initial_angle = (-0.5, 0.8, 0.0)
let initial_move = (0.0, 0.15, 2.0)

let generation = ref 0

let loop f =
  incr generation;
  let current_generation = !generation in
  let rec aux x =
    f x;
    if current_generation = !generation then
      Window.request_animation_frame window aux;
  in aux 0.0

let orthographic = false

(* return an angle between -pi and pi *)
let normalize_angle x =
 let open Math in
 if x >= 0.0 then
   let x = x -. (floor (x /. (2.0 *. pi))) *. 2.0 *. pi in
   if x > pi then
     x -. 2.0 *. pi
   else x
 else
   let x = x -. (ceil (x /. (2.0 *. pi))) *. 2.0 *. pi in
   if x < -. pi then
     x +. 2.0 *. pi
   else x

let position canvas evt =
  let rect = Element.get_bounding_client_rect canvas in
  let scale_x = Rect.width rect /. float (Canvas.width canvas) in
  let scale_y = Rect.height rect /. float (Canvas.height canvas) in
  let left = Rect.left rect in
  let top = Rect.top rect in
    (float (Event.clientX evt) -. left) /. scale_x,
    (float (Event.clientY evt) -. top) /. scale_y


let initialize canvas height width fps gl repere_model graph_model ({Surface.bounds = { z_max; z_min; _}; _} as surface) cube face_textures callback on_click =
  let aspect = width /. height in
  let pointer = ref (0.0, 0.0) in
  let angle = ref initial_angle in
  let move = ref initial_move in
  let scale = ref 1.0 in
  let moving = ref false in

  let open Event in
  let get_position = position canvas in
  add_event_listener canvas mousemove (fun evt ->
    prevent_default evt;
    let x, y = get_position evt in
    let x = 2.0 *. x /. width -. 1.0 in
    let y = 1.0 -. 2.0 *. y /. height in
    let button = buttons evt in
    if button > 0 then begin
      if !moving then begin
        let dx, dy =
          let x', y' = !pointer in
          x -. x', y -. y'
        in
        if button = 1 then begin
          let tx, ty, tz = !angle in
          let ty = normalize_angle (ty -. dx) in
          let tx = normalize_angle (tx +. dy) in
          let tx = max (-0.5 *. Math.pi) (min (0.5 *. Math.pi) tx) in
          angle := tx, ty, tz;
        end else if button = 2 then begin
          let tx, ty, tz = !move in
          let tx = tx +. dx in
          let ty = ty +. dy in
          move := tx, ty, tz;
        end
      end;
      moving := true;
    end else moving := false;
    pointer := (x,y));

  add_event_listener canvas dblclick (fun _ ->
    on_click ()
  );

  add_event_listener canvas wheel (fun evt ->
    prevent_default evt;
    let speed = 0.1 *. (1.0 +. 0.0 *. (z_max -. z_min)) in
    let y = if deltaY evt > 0.0 then speed else -. speed in
    if orthographic then begin
      scale := exp (log !scale +. y);
    end else begin
      let tx, ty, tz = !move in
      let tz =
        let tz' = tz +. y in
        if 0.5 < tz' && tz' < 3.0 then
          tz' else tz
      in
      move := tx, ty, tz;
    end);

  let previous_time = ref 0.0 in
  let fps_counter = ref 0 in
  let update_freq = 30 in
  let state = ref (!angle, !move, !scale, !pointer) in
  loop (fun clock ->
    incr fps_counter;
    if !fps_counter = update_freq then begin
      let t = (clock -. !previous_time) /. (float update_freq) in
      previous_time := clock;
      fps_counter := 0;
      Node.set_text_content fps
        (Printf.sprintf "%.2f fps" (1000.0 /. t));
    end;
    let new_state = !angle, !move, !scale, !pointer in
    if new_state <> !state then begin
      state := new_state;
      draw_scene gl aspect repere_model graph_model clock cube face_textures surface !angle !move !scale !pointer callback
    end)


let progress_bars () =
    let progress_bars = create "div" ~class_name:"progress" in
    let stack = Stack.create () in
    let add_bar () =
      let bar = create "div" ~class_name:"bar" in
      let inner = create "div" ~class_name:"inner" in
      Node.append_child progress_bars bar;
      Node.append_child bar inner;
      Stack.push (bar, inner) stack
    in
    let width_of_float x = Printf.sprintf "width:%2.2f%%;" (100.0 *. x) in
    object
      method element = progress_bars

      method status text =
        print_endline text;
        if Stack.is_empty stack then add_bar ();
        configure_element ~text (snd (Stack.top stack))

      method progress x =
        if Stack.is_empty stack then add_bar ();
        configure_element ~style:(width_of_float x) (snd (Stack.top stack))

      method push = add_bar ()

      method pop = try
          let bar, _ = Stack.pop stack in
          Node.remove_child progress_bars bar
        with _ -> ()
    end

type layout = {
  width: int;
  height: int;
  x_axis_label: string;
  y_axis_label: string;
  z_axis_label: string;
}

let list_init n f =
  let rec aux acc k =
    if k >= n then List.rev acc else aux ((f k) :: acc) (k + 1)
  in aux [] 0


let compute_projections points (x,z) =
  if Array.length points < 2 || Array.length points.(0) < 2 then
    None
  else
    let dim1 = Array.length points in
    let dim2 = Array.length points.(0) in
    let proj1, proj2, x, z =
       let x1,_,_ = points.(0).(0) in
       let x2,_,_ = points.(1).(0) in
       let x3,_,_ = points.(0).(1) in
       if x2 -. x1 > x3 -. x1 then
         (fun (x,y,z) -> x, (z, y)),
         (fun (x,y,z) -> z, (x, y)),
         x, z
       else
         (fun (x,y,z) -> z, (x, y)),
         (fun (x,y,z) -> x, (z, y)),
         z, x
    in
    let first_proj =
      let before =
        let result = ref 0 in
        Array.iteri (fun i row ->
            let x', _ = proj1 row.(0) in
            if x' < x then
              result := i) points;
        !result
      in
      if before = dim1 - 1 then None else
        let after = before + 1 in
        let prev, _ = proj1 points.(before).(0) in
        let next, _ = proj1 points.(after).(0) in
        let t = (x -. prev) /. (next -. prev) in
        let result =
          list_init dim2
            (fun k ->
               let _, (z1, y1) = proj1 points.(before).(k) in
               let _, (z2, y2) = proj1 points.(after).(k) in
               let interp x1 x2 = x1 *. t +. x2 *. (1.0 -. t) in
               interp z1 z2, interp y1 y2)
        in
        Some result
    in
    let second_proj =
      let before =
        let result = ref 0 in
        Array.iteri (fun j p ->
            let z', _ = proj2 p in
            if z' < z then
              result := j) points.(0);
        !result
      in
      if before = dim2 - 1 then None else
        let after = before + 1 in
        let prev, _ = proj2 points.(0).(before) in
        let next, _ = proj2 points.(0).(after) in
        let t = (z -. prev) /. (next -. prev) in
        let result =
          list_init dim1
            (fun k ->
               let _, (x1, y1) = proj2 points.(k).(before) in
               let _, (x2, y2) = proj2 points.(k).(after) in
               let interp x1 x2 = x1 *. t +. x2 *. (1.0 -. t) in
               interp x1 x2, interp y1 y2)
        in
        Some result
    in
    match first_proj, second_proj with
    | Some r1, Some r2 -> Some (r1, r2)
    | _ -> None

let new_plot {width; height; x_axis_label; y_axis_label; z_axis_label; _} ?(on_click=ignore) data =
  let main = create "div" in
  let progress_bars = progress_bars () in
  Node.append_child main (progress_bars # element);
  let context = (progress_bars :> context) in
  let canvas = Document.create_html_canvas document in
  Element.set_class_name canvas "webgl_plot";
  Event.add_event_listener canvas Event.contextmenu Event.prevent_default;
  let fps = create "div" in
  configure_element ~attributes:["width", string_of_int width; "height", string_of_int height] canvas;
  let gl =
    match get_context canvas WebGl with
    | Some gl -> gl
    | None ->
      match get_context canvas Experimental with
      | Some gl -> gl
      | None -> error "webgl is not supported"
  in
  enable gl (_DEPTH_TEST_ gl);
  depth_func gl (_LEQUAL_ gl);
  Html.Canvas.WebGl.line_width gl 4.0;

  let overlap = create ~class_name:"overlap" "div" in
  let container = create ~class_name:"container" "div" in
  Node.append_child main overlap;
  Node.append_child overlap canvas;
  Node.append_child overlap container;
  Node.append_child main fps;

  let floating_div = create ~class_name:"floatingDiv" "div" in
  Node.append_child container floating_div;
  let last_intersection = ref None in
  let callback (x,y) intersection =
    last_intersection := intersection;
    Element.set_attribute floating_div "style" (Printf.sprintf "position:absolute; left: %.2f%%; top: %.2f%%;" (50.0 *. (1.0 +. x)) (50.0 *. (1.0 -. y)));
    match intersection with
    | None ->
       Element.set_attribute canvas "style" "";
       Node.set_text_content floating_div ""
    | Some p -> let (x,y,z) = Math.Vector.to_three p in
       Element.set_attribute canvas "style" "cursor: none;";
       Node.set_text_content floating_div (Printf.sprintf "%f, %f, %f" x z y)
  in
  let on_click () =
    match !last_intersection with
    | Some p -> on_click (Math.Vector.to_three p)
    | None -> ()
  in
  let thread =
    begin
      context # status "Initializing ...";
      delay () >>= fun () -> begin
        Surface.from_grid context data >>= fun ({Surface.bounds; _} as surface) ->
        let repere_model = RepereModel.initialize gl in
        RepereModel.load repere_model;

        let new_texture canvas =
          let texture = create_texture gl in
          bind_texture gl (_TEXTURE_2D_ gl) texture;
          tex_image_2D gl (_TEXTURE_2D_ gl) 0 (_RGBA_ gl) (_RGBA_ gl) (type_unsigned_byte gl)  (`Canvas canvas);
          tex_parameteri gl (_TEXTURE_2D_ gl) (_TEXTURE_MAG_FILTER_ gl) (_LINEAR_ gl);
          tex_parameteri gl (_TEXTURE_2D_ gl) (_TEXTURE_MIN_FILTER_ gl) (_LINEAR_ gl);
          texture
        in
        let cube_texture =
          let canvas = Textures.create_face_texture document in
          new_texture canvas
        in
        let x_texture =
          let canvas = Textures.create_ticks_texture document x_axis_label
              (Textures.uniform_ticks 10 bounds.x_min bounds.x_max) in
          new_texture canvas
        in
        let y_texture =
          let canvas = Textures.create_ticks_texture document y_axis_label
              (Textures.uniform_ticks 10 bounds.y_min bounds.y_max) in
          new_texture canvas
        in
        let z_texture =
          let canvas = Textures.create_ticks_texture document z_axis_label
              (Textures.uniform_ticks 10 bounds.z_min bounds.z_max) in
          new_texture canvas
        in
        let graph_model = GraphModel.initialize gl in
        let cube = RepereModel.build_cube bounds cube_texture in
        initialize canvas (float height) (float width) fps gl repere_model graph_model surface cube [| x_texture; y_texture; z_texture |] callback on_click;
        Node.remove_child main (progress_bars # element);
        return ()
      end
    end
  in
  on_failure thread (fun e -> Printf.printf "Failure: '%s'\n%!" (Printexc.to_string e));
  on_success thread (fun () -> Printf.printf "Success \\o/\n%!");
  return main
