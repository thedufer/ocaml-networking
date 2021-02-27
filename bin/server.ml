open! Core
open! Async
open Sdn_local_protocol

open Deferred.Or_error.Let_syntax

module Message_log = struct
  type t = {
    from : Node.Id.t * int;
    to_  : Node.Id.t * int;
    sent : char list;
    received : char list list;
    delivered : bool;
  } [@@deriving sexp_of]
end

let update (state, _, _, _) ~f =
  let%bind new_state = Deferred.return (f !state) in
  state := new_state;
  State.save !state

let register_connection ((state : State.t ref), w_bag, messages, conn) id =
  let%bind () =
    if Bag.exists w_bag ~f:(fun (_, id') -> Node.Id.equal id id') then
      Deferred.Or_error.error_s [%message "already connected" (id : Node.Id.t)]
    else
      return ()
  in
  let (ret_pipe, w) = Pipe.create () in
  let%bind (r, _) =
    Rpcs.Register_response.dispatch conn ()
    |> Deferred.map ~f:Or_error.join
  in
  let elt = Bag.add w_bag (w, id) in
  Pipe.closed w
  |> Deferred.map ~f:(fun () -> Bag.remove w_bag elt)
  |> don't_wait_for;
  Pipe.iter_without_pushback r ~f:(fun (from_msg : Message.t) ->
      Connection.get_connected_port_and_connection (!state).connections (id, from_msg.port)
      |> Option.iter  ~f:(fun (to_id, to_port, (conn : Connection.t)) ->
          let to_msgs =
            Connection.Transformations.map_data
              conn.transformations
              conn.extra_bits
              from_msg.data
            |> List.map ~f:(fun data ->
                { Message.
                  port = to_port;
                  data;
                })
          in
          let delivered = ref false in
          Bag.iter w_bag ~f:(fun (w_pipe, id) ->
              if Node.Id.equal id to_id then
                (delivered := true;
                 List.iter to_msgs ~f:(fun to_msg ->
                     Pipe.write_if_open w_pipe to_msg
                     |> don't_wait_for)));
          Queue.enqueue messages
            { Message_log.
              from = (id, from_msg.port)
            ; to_ = (to_id, to_port)
            ; sent = from_msg.data
            ; received = List.map to_msgs ~f:(fun msg -> msg.data)
            ; delivered = !delivered
            };
        ))
  |> don't_wait_for;
  return ret_pipe

let implementations =
  let implementations =
    [ Rpcs.Add_node.implement (fun state (id, ports) ->
          update state ~f:(fun s -> State.add_node s id ports));
      Rpcs.Drop_node.implement (fun state node_id ->
          update state ~f:(fun s -> State.drop_node s node_id));
      Rpcs.Add_connection.implement (fun state connection ->
          update state ~f:(fun s -> State.add_connection s connection));
      Rpcs.Drop_connection.implement (fun state connection ->
          update state ~f:(fun s -> State.drop_connection s connection));
      Rpcs.Register.implement register_connection;
    ]
  in
  Rpc.Implementations.create_exn ~implementations ~on_unknown_rpc:`Close_connection

let start_http_server ~port state_ref messages =
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
    | (`GET, [""]) ->
      let data = State.to_dot_format state in
      let%bind dot_proc = Async_unix.Process.create ~stdin:data ~prog:"dot" ~args:["-Tsvg"] () in
      let%bind svg_graph = Async_unix.Process.collect_stdout_and_wait dot_proc in
      let str =
        let open Html in
        let message_html =
          Queue.to_list messages
          |> List.map ~f:(fun message ->
              div [
                Message_log.sexp_of_t message
                |> Sexp.to_string
                |> text
              ])
          |> div
        in
        html [head []; body [
            text "Here it is:";
            raw svg_graph;
            message_html;
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

let run_server_command =
  let open Command.Let_syntax in
  Command.async_or_error ~summary:"start the server"
    [%map_open
      let http_port =
        flag "-http-port" (optional_with_default 8080 int)
          ~doc:"INT http port"
      in
      fun () ->
        let open Deferred.Or_error.Let_syntax in
        let%bind state = State.load () in
        let state_ref = ref state in
        let w_bag = Bag.create () in
        let messages = Queue.create () in
        let where_to_listen = Tcp.Where_to_listen.of_port state.server_port in
        let%bind server =
          Rpc.Connection.serve ~implementations () ~where_to_listen
            ~initial_connection_state:(fun _ conn -> (state_ref, w_bag, messages, conn))
          |> Deferred.ok
        in
        let%bind http_server =
          start_http_server ~port:http_port state_ref messages |> Deferred.ok
        in
        let error_after_close server type_ =
          let%bind () = Tcp.Server.close_finished server |> Deferred.ok in
          Deferred.Or_error.errorf "%s closed unexpectedly" type_
        in
        Deferred.any [
          error_after_close server "rpc server";
          error_after_close http_server "http server";
        ]
    ]

let init_command =
  let open Command.Let_syntax in
  Command.async_or_error ~summary:"init a state"
    [%map_open
      let () = return ()
      in
      fun () ->
        let open Deferred.Or_error.Let_syntax in
        let%bind server_port =
          (* Get a random port that isn't already being used and claim it. *)
          let where_to_listen = Tcp.Where_to_listen.of_port_chosen_by_os in
          let%bind server =
            Tcp.Server.create ~on_handler_error:`Ignore where_to_listen (fun _ _ _ -> Deferred.return ())
            |> Deferred.ok
          in
          let%bind () =
            Tcp.Server.close server
            |> Deferred.ok
          in
          Tcp.Server.listening_on server
          |> return
        in
        let state = State.init ~server_port in
        let%bind () = State.save state in
        return ()
    ]

let command =
  Command.group ~summary:"sdn-local server" [
    ("init", init_command);
    ("run-server", run_server_command);
  ]
