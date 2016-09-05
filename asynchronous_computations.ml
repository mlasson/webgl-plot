include Lwt

class type context = object
  method status : string -> unit
  method progress : float -> unit
  method push: unit
  method pop: unit
end

let console_context = object
  method status x = print_endline x
  method progress x = Printf.printf "%2.2f%%\n%!" (100.0 *. x)
  method push = print_endline "-->"
  method pop = print_endline "<--"
end

let delay () =
  let res, t = wait () in
  Js_bindings.set_timeout (wakeup t) 0.0 |> ignore;
  res

let rec range f min max =
  if min <= max then
    (f min) >>= (fun () -> range f (min + 1) max)
  else return ()

let rec range_chunks size f first last =
  if first <= last then
    let next = min (first + size) last in
    return (for k = first to next do
      f k
    done) >>= (fun () -> range_chunks size f (next + 1) last)
  else return ()

let map_chunks ?(delay = delay) size f l =
  let rec aux acc k l =
    if k = 0 then begin
      delay () >>= (fun () -> aux acc size l)
    end else begin
      match l with
      | [] -> return (List.rev acc)
      | hd :: tl -> aux ((f hd) :: acc) (k - 1) tl
    end
  in
  aux [] size l

let iter_chunks ?(delay = delay) size f l =
  let rec aux k l =
    if k = 0 then begin
      delay () >>= (fun () -> aux size l)
    end else begin
      match l with
      | [] -> return ()
      | hd :: tl ->
        f hd;
        aux (k - 1) tl
    end
  in
  aux size l

let delay_chunks context size max =
  let cpt = ref 0 in
  let max = float max in
  fun () -> cpt := !cpt + size; context#progress ((float !cpt) /. max); delay ()

let hashtbl_to_list context table =
  context # push;
  context # progress 0.0;
  delay ()
  >>= fun () ->
  return (List.sort_uniq compare
            (Hashtbl.fold (fun k _ acc -> k :: acc) table []))
  >>= fun keys ->
  let n = List.length keys in
  let chunks = 100 in
  let delay = delay_chunks context chunks n in
  map_chunks chunks ~delay (fun key -> key, Hashtbl.find_all table key) keys
  >>= fun result -> context # pop; return result


