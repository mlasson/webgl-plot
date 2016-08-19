open Js_bindings
open Models
open Html
open Canvas.WebGl
open Helper
open Asynchronous_computations

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

let initialize canvas height width fps gl repere_model graph_model surface cube =
  let aspect = width /. height in
  let pointer = ref (0.0, 0.0) in
  let angle = ref (0., 0., 0.) in
  let move = ref (0., 0., 2.) in
  let scale = ref 1.0 in
  let moving = ref false in

  let open Event in
  add_event_listener canvas mousemove (fun evt ->
    prevent_default evt;
    let x = 2.0 *. (float (offsetX evt)) /. width -. 1.0 in
    let y = 1.0 -. 2.0 *. (float (offsetY evt)) /. height in
    let button = buttons evt in
    if button > 0 then begin
      if !moving then begin
        let dx, dy =
          let x', y' = !pointer in
          x -. x', y -. y'
        in
        if button = 1 then begin
          let tx, ty, tz = !angle in
          let ty = normalize_angle (ty +. dx) in
          let tx = normalize_angle (tx -. dy) in
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

  add_event_listener canvas wheel (fun evt ->
    prevent_default evt;
    let y = deltaY evt /. height in
    if orthographic then begin
      Printf.printf "scale + %f = %f\n%!" y !scale;
      scale := exp (log !scale +. y);
    end else begin
      let tx, ty, tz = !move in
      let tz = tz +. y in
      move := tx, ty, tz;
    end
    );

  let previous_time = ref 0.0 in
  let fps_counter = ref 0 in
  let update_freq = 30 in
  loop (fun clock ->
    incr fps_counter;
    if !fps_counter = update_freq then begin
      let t = (clock -. !previous_time) /. (float update_freq) in
      previous_time := clock;
      fps_counter := 0;
      Node.set_text_content fps
        (Printf.sprintf "%.2f fps" (1000.0 /. t));
    end;
    draw_scene gl aspect repere_model graph_model clock cube surface !angle !move !scale !pointer)


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
  height: int
}

let new_plot {width; height; _} data =
  let main = create "div" in
  let progress_bars = progress_bars () in
  Node.append_child main (progress_bars # element);
  let context = (progress_bars :> context) in
  let canvas = Document.create_html_canvas document in
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

  Node.append_child main canvas;
  Node.append_child main fps;

  let thread =
    begin
      context # status "Initializing ...";
      delay () >>= fun () -> begin
        Surface.flat context data >>= fun ({Surface.bounds; _} as surface) ->
        let repere_model = RepereModel.initialize gl in
        RepereModel.load repere_model;

        let textures = Array.map (fun descr ->
            let texture_canvas =
              Textures.create_grid_texture document
                (Textures.uniform_ticks 10 bounds.x_min bounds.x_max)
                (Textures.uniform_ticks 10 bounds.y_min bounds.y_max)
                (Textures.uniform_ticks 10 bounds.z_min bounds.z_max) descr
            in
            if false then Node.append_child main texture_canvas;
            let texture = create_texture gl in
            bind_texture gl (_TEXTURE_2D_ gl) texture;
            tex_image_2D gl (_TEXTURE_2D_ gl) 0 (_RGBA_ gl) (_RGBA_ gl) (type_unsigned_byte gl)  (`Canvas texture_canvas);
            tex_parameteri gl (_TEXTURE_2D_ gl) (_TEXTURE_MAG_FILTER_ gl) (_LINEAR_ gl);
            tex_parameteri gl (_TEXTURE_2D_ gl) (_TEXTURE_MIN_FILTER_ gl) (_LINEAR_ gl);
            texture)
            [|
              [ `Bottom, false, `Left; `Bottom, false, `Bottom; `Back, false, `Left ];
              [ `Bottom, false, `Left; `Bottom, false, `Bottom; `Right, false, `Right ];
              [ `Bottom, true, `Left; `Bottom, false, `Top; `Right, false, `Left ];
              [ `Bottom, true, `Left; `Bottom, false, `Top; `Front, false, `Right ];

              [ `Bottom, true, `Right; `Bottom, true, `Top; `Front, false, `Left ];
              [ `Bottom, true, `Right; `Bottom, true, `Top; `Left, false, `Right ];
              [ `Bottom, false, `Right; `Bottom, true, `Bottom; `Left, false, `Left ];
              [ `Bottom, false, `Right; `Bottom, true, `Bottom; `Back, false, `Right ];

              [ `Top, false, `Left; `Top, true, `Top; `Back, false, `Left ];
              [ `Top, false, `Left; `Top, true, `Top; `Right, false, `Right ];
              [ `Top, true, `Left; `Top, true, `Bottom; `Right, false, `Left ];
              [ `Top, true, `Left; `Top, true, `Bottom; `Front, false, `Right ];

              [ `Top, true, `Right; `Top, false, `Bottom; `Front, false, `Left ];
              [ `Top, true, `Right; `Top, false, `Bottom; `Left, false, `Right ];
              [ `Top, false, `Right; `Top, false, `Top; `Left, false, `Left ];
              [ `Top, false, `Right; `Top, false, `Top; `Back, false, `Right ];

            |]
        in
        let graph_model = GraphModel.initialize gl in
        let cube = RepereModel.build_cube bounds textures in
        initialize canvas (float height) (float width) fps gl repere_model graph_model surface cube;
        Node.remove_child main (progress_bars # element);
        return ()
      end
    end
  in
  on_failure thread (fun e -> Printf.printf "Failure: '%s'\n%!" (Printexc.to_string e));
  on_success thread (fun () -> Printf.printf "Success \\o/\n%!");
  return main
