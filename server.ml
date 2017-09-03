open! Core
open! Async
open Protocol
open Deferred.Or_error.Let_syntax

let update state ~f =
  let%bind new_state = Deferred.return (f !state) in
  state := new_state;
  State.save !state

let implementations =
  let implementations =
    [ Rpcs.Add_node.implement (fun (state : State.t ref) node ->
          update state ~f:(fun s -> State.add_node s node));
      Rpcs.Drop_node.implement (fun (state : State.t ref) node_id ->
          update state ~f:(fun s -> State.drop_node s node_id));
      Rpcs.Add_connection.implement (fun (state : State.t ref) connection ->
          update state ~f:(fun s -> State.add_connection s connection));
      Rpcs.Drop_connection.implement (fun (state : State.t ref) connection ->
          update state ~f:(fun s -> State.drop_connection s connection));
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
        let where_to_listen = Tcp.on_port state.server_port in
        let%bind server =
          Rpc.Connection.serve ~implementations () ~where_to_listen
            ~initial_connection_state:(fun _ _ -> state_ref)
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

let () = Command.run command
