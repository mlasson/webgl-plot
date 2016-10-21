(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

open Js_windows

let error s =
  print_endline ("Error: "^s);
  failwith s

let document = Window.document window

let removeAll element =
  while Element.has_child_nodes element do
    Element.remove_child element (Element.last_child element)
  done

let element_of_id id =
  match Document.get_element_by_id document id with
  | Some element -> element
  | None -> error (Printf.sprintf "Element of id '%s' not found" id)

let hide element =
  Element.set_attribute element "style" "display: none"

let show element =
  Element.remove_attribute element "style"

let configure_element ?text ?class_name ?style ?(attributes = []) element =
  (match text with
   | Some text -> Element.set_text_content element text
   | _ -> ());
  (match style with
   | Some style -> Element.set_attribute element "style" style
   | _ -> ());
  (match class_name with
   | Some class_name -> Element.set_class_name element class_name
   | _ -> ());
  List.iter (fun (name, value) -> Element.set_attribute element name value) attributes

let create ?text ?class_name ?style ?attributes ?parent name children =
  let element = Document.create_element document name in
  configure_element ?text ?class_name ?style ?attributes element;
  List.iter (Element.append_child element) children;
  (match parent with Some p -> Element.append_child p element | _ -> ());
  element
