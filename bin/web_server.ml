open Core
open Async
open Sdn_local_protocol
open Deferred.Or_error.Let_syntax

let sse_updates (headers : Httpaf.Headers.t) messages =
  let sse_acceptable =
    Httpaf_caged.Accept.extract headers
    |> List.exists ~f:(fun (media_type, _q) ->
           match media_type with
           | Type ("text", "event-stream") -> true
           | _ -> false)
  in
  if sse_acceptable then
    let messages_pipe, w = Pipe.create () in
    let messages, messages_pipe' = Pipe_buffer.get messages in
    List.iter messages ~f:(Pipe.write_without_pushback w);
    don't_wait_for
      (let%bind.Deferred () = Pipe.transfer_id messages_pipe' w in
       Pipe.close w; Deferred.unit);
    let string_pipe =
      let data =
        Pipe.map messages_pipe ~f:(fun update ->
            sprintf !"data: %{Sexp#mach}\r\n\r\n" (Message_log.sexp_of_t update))
      in
      let keepalives =
        let r, w = Pipe.create () in
        Clock.every' ~stop:(Pipe.closed w) Time.Span.minute (fun () ->
            Pipe.write_if_open w ":\r\n");
        r
      in
      Pipe.concat
        [
          Pipe.singleton "retry: 1500\r\n\r\n";
          Pipe.interleave [ data; keepalives ];
        ]
    in
    Httpaf_caged.Server.respond_with_pipe
      ~headers:
        (Httpaf.Headers.of_list
           [ ("content-type", "text/event-stream"); ("X-Accel-Buffering", "no") ])
      string_pipe
    |> Deferred.ok
  else
    Httpaf_caged.Server.respond_string ~status:`Not_acceptable
      "This route only serves SSE [text/event-stream]s"
    |> Deferred.ok

let start ~port state_ref messages =
  let request_handler ~body:_ (request:Httpaf_caged.Request.t) =
    let state = !state_ref in
    let target =
      String.strip ~drop:(Char.equal '/') request.target
      |> String.split ~on:'/'
    in
    match (request.meth, target) with
    | (`GET, ["graph.dot"]) ->
      let data = State.to_dot_format state in
      Httpaf_caged.Server.respond_string data |> Deferred.ok
    | (`GET, ["graph.svg"]) ->
      let data = State.to_dot_format state in
      let%bind p = Async_unix.Process.create ~stdin:data ~prog:"dot" ~args:["-Tsvg"] () in
      let%bind o = Async_unix.Process.collect_stdout_and_wait p in
      Httpaf_caged.Server.respond_string o |> Deferred.ok
    | (`GET, ["messages"]) ->
      sse_updates request.headers messages
    | (`GET, ["main.js"]) ->
      Httpaf_caged.Server.respond_with_file
        ~headers:(Httpaf.Headers.of_list [("content-type", "text/javascript")])
        (Filename.dirname Sys.executable_name ^/ "web-client/main.bc.js")
      |> Deferred.ok
    | (`GET, ["main.css"]) ->
      Httpaf_caged.Server.respond_string
        ~headers:(Httpaf.Headers.of_list [("content-type", "text/css")])
        {|
        .message {
          font-family: monospace;
        }
        |}
      |> Deferred.ok
    | (`GET, [""]) ->
      let data = State.to_dot_format state in
      let%bind dot_proc = Async_unix.Process.create ~stdin:data ~prog:"dot" ~args:["-Tsvg"] () in
      let%bind svg_graph = Async_unix.Process.collect_stdout_and_wait dot_proc in
      let str =
        let open Html in
        html [
          head [
            script ~attrs:[("src", "/main.js")] []
          ; link ~attrs:[("rel", "stylesheet"); ("href", "/main.css")] []
          ]
        ; body [
            text "Here it is:";
            raw svg_graph;
            div ~attrs:[("id", "for-client")] []
          ]]
        |> to_string
      in
      Httpaf_caged.Server.respond_string str |> Deferred.ok
    | _ ->
      let str =
        sprintf "unknown: %s"
          ([%sexp_of: string list] target
           |> Sexp.to_string)
      in
      Httpaf_caged.Server.respond_string str |> Deferred.ok
  in
  let request_handler ~body _sock req =
    match%bind.Deferred request_handler ~body req with
    | Ok response -> Deferred.return response
    | Error err ->
      Httpaf_caged.Server.respond_string
        ~status:`Internal_server_error
        (sprintf !"%{Error#hum}\n" err)
  in
  let on_handler_error _ ?request:_ error start_response =
    let open Httpaf in
    let response_body = start_response Headers.empty in
    begin
      match error with
      | `Exn exn ->
        Body.write_string response_body (Exn.to_string exn)
      | #Status.standard as error ->
        Body.write_string response_body (Status.default_reason_phrase error)
    end;
    Body.write_string response_body "\n";
    Body.close_writer response_body
  in
  Httpaf_caged.Server.create
    ~on_handler_error
    (Tcp.Where_to_listen.of_port port)
    request_handler
