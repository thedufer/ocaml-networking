open! Core_kernel
open Async_kernel
open Sdn_local_protocol_kernel
open Js_of_ocaml

let sse () =
  let source = new%js EventSource.eventSource (Js.string "/messages") in
  let r, w = Pipe.create () in
  let (_ : Dom.event_listener_id) =
    EventSource.addEventListener source (Dom.Event.make "message")
      (Dom.handler (fun evt ->
           Pipe.write_without_pushback_if_open w (Js.to_string evt##.data);
           Js._true))
      Js._false
  in
  r

let start () =
  let messages = sse () in
  let container =
    Dom_html.document##getElementById (Js.string "for-client")
    |> Js.Opt.to_option
    |> Option.value_exn
  in
  don't_wait_for (Pipe.iter_without_pushback messages ~f:(fun msg ->
      let msg = Message_log.t_of_sexp (Sexp.of_string msg) in
      let new_node = Dom_html.document##createElement (Js.string "div") in
      new_node##.textContent := (Js.string (Sexp.to_string (Message_log.sexp_of_t msg)) |> Js.Opt.return);
      Dom.appendChild container new_node));
  Async_js.init ()

let () =
  Dom_html.window##.onload :=
    Dom.handler (fun _ -> start (); Js.bool true)
