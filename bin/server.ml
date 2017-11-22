open! Core
open! Async
open Sdn_local_protocol

open Deferred.Or_error.Let_syntax

let update (state, _, _) ~f =
  let%bind new_state = Deferred.return (f !state) in
  state := new_state;
  State.save !state

let register_connection ((state : State.t ref), w_bag, conn) id =
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
          Bag.iter w_bag ~f:(fun (w_pipe, id) ->
              if Node.Id.equal id to_id then
                List.iter to_msgs ~f:(fun to_msg ->
                    Pipe.write_if_open w_pipe to_msg
                    |> don't_wait_for))))
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

let run_server_command =
  let open Command.Let_syntax in
  Command.async_or_error' ~summary:"start the server"
    [%map_open
      let () = return ()
      in
      fun () ->
        let open Deferred.Or_error.Let_syntax in
        let%bind state = State.load () in
        let state_ref = ref state in
        let w_bag = Bag.create () in
        let where_to_listen = Tcp.on_port state.server_port in
        let%bind server =
          Rpc.Connection.serve ~implementations () ~where_to_listen
            ~initial_connection_state:(fun _ conn -> (state_ref, w_bag, conn))
          |> Deferred.ok
        in
        Tcp.Server.close_finished server
        |> Deferred.ok
    ]

let init_command =
  let open Command.Let_syntax in
  Command.async_or_error' ~summary:"init a state"
    [%map_open
      let () = return ()
      in
      fun () ->
        let open Deferred.Or_error.Let_syntax in
        let%bind server_port =
          (* Get a random port that isn't already being used and claim it. *)
          let where_to_listen = Tcp.on_port_chosen_by_os in
          let%bind server =
            Tcp.Server.create where_to_listen (fun _ _ _ -> Deferred.return ())
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
