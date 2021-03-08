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
      new_node##.classList##add (Js.string "message");
      let rendered_sent =
        let printable =
          List.map msg.sent ~f:(fun c ->
              if Char.is_print c then c else '.')
          |> String.of_char_list
        in
        let hex =
          List.map msg.sent ~f:(fun c ->
              let i = Char.to_int c in
              sprintf "%02x" i)
          |> List.chunks_of ~length:2
          |> List.intersperse ~sep:[" "]
          |> List.concat
          |> String.concat
        in
        printable ^ "  " ^ hex
      in
      let content =
        sprintf !"%{Node.Id}#%d->%{Node.Id}#%d: %s" (fst msg.from) (snd msg.from) (fst msg.to_) (snd msg.to_) rendered_sent
      in
      new_node##.textContent := (Js.string content |> Js.Opt.return);
      Dom.appendChild container new_node));
  Async_js.init ()

let () =
  Dom_html.window##.onload :=
    Dom.handler (fun _ -> start (); Js.bool true)
