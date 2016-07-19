 open Js_core

let error s =
  alert ("Error: "^s);
  failwith s

let document = Window.document window

let removeAll element =
    while
     match Node.last_child element with
     | Some child -> Node.remove_child element child; true
     | None -> false
    do () done

  let element_of_id id =
    match Document.get_element_by_id document id with
    | Some element -> element
    | None -> error (Printf.sprintf "Element of id '%s' not found" id)

  let input_of_id id =
    match Html.retype (element_of_id id) with
    | `Input input -> input
    | _ ->
      error (Printf.sprintf "Element of id '%s' should be an input element." id)

  let hide element =
    Element.set_attribute element "style" "display: none"

  let show element =
    Element.remove_attribute element "style"

  let configure_element ?text ?class_name ?style ?(attributes = []) element =
    (match text with
     | Some text -> Node.set_text_content element text
     | _ -> ());
    (match style with
     | Some style -> Element.set_attribute element "style" style
     | _ -> ());
    (match class_name with
     | Some class_name -> Element.set_class_name element class_name
     | _ -> ());
    List.iter (fun (name, value) -> Element.set_attribute element name value) attributes

  let create ?text ?class_name ?style ?attributes name =
    let element = Document.create_element document name in 
    configure_element ?text ?class_name ?style ?attributes element;
    element

  let compile_shader gl source shader_type = 
    let open Html.Canvas.WebGl in
    let shader = create_shader gl shader_type in
    shader_source gl shader source;
    compile_shader gl shader;
    let success = get_shader_parameter gl shader (shader_compile_status gl) in
    if not success then
      error (Printf.sprintf "Unable to compile shader '%s'" 
               (get_shader_info_log gl shader));
    shader

  let compile_program gl vertex_shader fragment_shader = 
    let open Html.Canvas.WebGl in
    let program = create_program gl in
    attach_shader gl program vertex_shader;
    attach_shader gl program fragment_shader;
    link_program gl program;
    let success =
      get_program_bool_parameter gl program (program_link_status gl) 
    in
    if not success then
      error (Printf.sprintf "Unable to program program '%s'"
               (get_program_info_log gl program));
    program
  
  let new_shader gl shader shader_type = 
    let shader_type =
      let open Html.Canvas.WebGl in
      match shader_type with
      | `Fragment -> fragment_shader_type gl
      | `Vertex -> vertex_shader_type gl
    in
    compile_shader gl shader shader_type


