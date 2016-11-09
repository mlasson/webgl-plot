(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

open Js_browser

module Math = Webgl_plot_math
module Helper = Webgl_plot_dom_helper

let initial_angle = (-0.5, 0.8, 0.0)
let initial_move = (0.0, 0.15, 2.0)

type state = {
  mutable aspect : float;
  mutable angle : float * float * float;
  mutable move : float * float * float;
  mutable dragging : bool;
  mutable pointer: float * float;
  mutable width: float;
  mutable height: float;

  mutable on_double_click: (unit -> unit);
  mutable pending_screenshots: (string -> unit) list;
}

class type context =
  object
    method mod_down : bool
    method new_textbox :
      < element : Js_browser.Element.t;
        set_position : float * float -> unit;
        set_text : string -> unit >
    method set_cursor_visibility : bool -> unit
  end

let generation = ref 0
let loop f =
  incr generation;
  let current_generation = !generation in
  let rec aux x =
    f x;
    if current_generation = !generation then
      Window.request_animation_frame window aux;
  in aux 0.0

let position canvas evt =
  let rect = Element.get_bounding_client_rect canvas in
  let scale_x = Rect.width rect in
  let scale_y = Rect.height rect in
  let left = Rect.left rect in
  let top = Rect.top rect in
  let x = (float (Event.client_x evt) -. left) /. scale_x in
  let y = (float (Event.client_y evt) -. top) /. scale_y in
  (* x/y positions between -1 and 1 : *)
  let x = 2.0 *. x -. 1.0 in
  let y = 1.0 -. 2.0 *. y in
  x, y

(* return an angle between -pi and pi *)
let normalize_angle x =
 let open Math in
 let two_pi = 2.0 *. pi in
 if x >= 0.0 then
   let x = x -. (floor (x /. two_pi)) *. two_pi in
   if x > pi then
     x -. two_pi
   else x
 else
   let x = x -. (ceil (x /. two_pi)) *. two_pi in
   if x < -. pi then
     x +. two_pi
   else x

let setup_webgl_context canvas =
  let open Webgl in
  let gl =
    match get_context canvas WebGl with
    | Some gl -> gl
    | None ->
      match get_context canvas Experimental with
      | Some gl -> gl
      | None -> Helper.error "webgl is not supported"
  in
  ignore (get_extension gl "OES_element_index_uint");
  gl

let create_webgl_canvas renderer =
  let canvas =
    let style = {css|
      -webkit-user-select: none;
      -moz-user-select: none;
      user-select: none;
      width: 100%;
      height: 100%;
    |css} in
    let attributes = [ "width", string_of_int 400; "height", string_of_int 400] in
    Helper.create ~attributes ~style "canvas" []
  in
  let container =
    let style = {css|
      position: absolute;
      left: 0px;
      top: 0px;
      width: 100%;
      height: 100%;
      z-index: 10;
      overflow: hidden;
      pointer-events:none;
    |css} in
    Helper.create ~style "div" [] in
  let fps_counter = Helper.create "div" [] in
  let overlap =
    let style = {css|
      position: relative;
      width:100%;
      height: 100%;
    |css} in
    Helper.create ~style "div" [canvas; container]
  in
  let main =
    let style = {css|
      width: 100%;
      height: 100%;
    |css} in
    Helper.create "div" ~style [overlap; fps_counter]
  in
  let state =
    {
      aspect = 1.0;
      angle = initial_angle;
      move = initial_move;
      dragging = false;
      pointer = (0., 0.);
      width = 0.0;
      height = 0.0;
      on_double_click = ignore;
      pending_screenshots = [];
    }
  in
  let new_textbox () =
    let style = {css|
      -webkit-user-select: none;
      -moz-user-select: none;
      user-select: none;
      position: absolute;
      text-shadow: -1px 0 white, 0 1px white, 1px 0 white, 0 -1px white;
    |css} in
    let element = Helper.create ~parent:container ~style "div" [] in
    object
      method element = element
      method set_text = Element.set_text_content element
      method set_position (x,y) =
       let style = Element.style element in
       Style.set_left style (Printf.sprintf "%.2f%%" (50.0 *. (1. +. x)));
       Style.set_top style (Printf.sprintf "%.2f%%" (50.0 *. (1. -. y)))
    end
  in

  (* Register all event listeners *)

  (* Disable context menu on right click: *)
  Element.add_event_listener canvas "contextmenu"
    (fun t -> Event.prevent_default t) true;

  begin (* Mouse move event: *)
    let open Event in

    Element.add_event_listener canvas "selectstart" (fun evt ->
        prevent_default evt;) true;

    Element.add_event_listener canvas "dblclick" (fun evt ->
        prevent_default evt;
        state.on_double_click ()) true;

    Element.add_event_listener canvas "mousemove" (fun evt ->
        prevent_default evt;
        let x,y = position canvas evt in
        let button = buttons evt in
        if button > 0 then begin
          if state.dragging then begin
            let dx, dy =
              let x', y' = state.pointer in
              x -. x', y -. y'
            in
            if button = 1 then begin
              let tx, ty, tz = state.angle in
              let ty = normalize_angle (ty -. dx) in
              let tx = normalize_angle (tx +. dy) in
              let tx = max (-0.5 *. Math.pi) (min (0.5 *. Math.pi) tx) in
              state.angle <- tx, ty, tz;
            end else if button = 2 then begin
              let tx, ty, tz = state.move in
              let tx = tx +. dx in
              let ty = ty +. dy in
              state.move <- tx, ty, tz;
            end
          end;
          state.dragging <- true;
        end else
          state.dragging <- false;
        state.pointer <- (x,y);) true;
    (* Wheel event *)
    Element.add_event_listener canvas "wheel" (fun evt ->
        prevent_default evt;
        let speed = 0.1 in
        let y = if delta_y evt > 0.0 then speed else -. speed in
        let tx, ty, tz = state.move in
        let tz =
          let tz' = tz +. y in
          if 0.8 < tz' && tz' < 3.0 then
            tz' else tz
        in
        state.move <- tx, ty, tz;) true;
  end;

  let gl = setup_webgl_context canvas in

  let mod_down = ref false in

  Element.add_event_listener (Document.body document) "keydown" (fun evt ->
      if Event.shift_key evt then begin
        Event.prevent_default evt;
        mod_down := true;
      end) true;

  Element.add_event_listener (Document.body document) "keyup" (fun evt ->
      if not (Event.shift_key evt) then begin
        Event.prevent_default evt;
        mod_down := false;
      end) true;

  let scene, next_frame = renderer gl
      (object
        method new_textbox = new_textbox ()
        method mod_down = !mod_down
        method set_cursor_visibility b =
          let style = Element.style canvas in
          Style.set_cursor style (if b then "auto" else "none")
      end)
  in

  (* Main loop : *)
  let fps = ref 0 in
  let previous_time = ref 0.0 in
  let update_freq = 30 in
  loop (fun clock ->
    let width, height =
      let rect = Element.get_bounding_client_rect canvas in
      Rect.width rect, Rect.height rect
    in
    if state.width <> width || state.height <> height then begin
      state.width <- width;
      state.height <- height;
      let h, w = int_of_float height, int_of_float width in
      Element.set_attribute canvas "height" (string_of_int h);
      Element.set_attribute canvas "width" (string_of_int w);
    end;
    state.aspect <- width /. height;

    incr fps;
    if !fps = update_freq then begin
      let t = (clock -. !previous_time) /. (float update_freq) in
      previous_time := clock;
      fps := 0;
      Element.set_text_content fps_counter
        (Printf.sprintf "%.2f fps" (1000.0 /. t));
      (* print_state state *)
    end;
    next_frame clock state;
    List.iter (fun f -> f (Canvas.to_data_url canvas)) state.pending_screenshots;
    state.pending_screenshots <- [];
    );
  main, container, state, scene

