open! Core
open! Async
open Sdn_local_protocol

let add_node_command =
  let open Command.Let_syntax in
  Command.async_or_error ~summary:"add a node"
    [%map_open
      let id = anon ("NEW-NODE-ID" %: string)
      and ports = anon ("PORTS-COUNT" %: int)
      in
      fun () ->
        let open Deferred.Or_error.Let_syntax in
        let%bind state = State.load () in
        let%bind conn =
          let port = state.server_port in
          Rpc.Connection.client
            (Tcp.Where_to_connect.of_host_and_port {host="localhost"; port})
          |> Deferred.Or_error.of_exn_result
        in
        let%bind () =
          let id = Node.Id.of_string id in
          Rpcs.Add_node.dispatch conn (id, ports)
          |> Deferred.map ~f:Or_error.join
        in
        printf "done\n";
        return ()
    ]

let drop_node_command =
  let open Command.Let_syntax in
  Command.async_or_error ~summary:"drop a node"
    [%map_open
      let id = anon ("NODE-ID" %: string)
      in
      fun () ->
        let open Deferred.Or_error.Let_syntax in
        let%bind state = State.load () in
        let%bind conn =
          let port = state.server_port in
          Rpc.Connection.client
            (Tcp.Where_to_connect.of_host_and_port {host="localhost"; port})
          |> Deferred.Or_error.of_exn_result
        in
        let%bind () =
          Rpcs.Drop_node.dispatch conn (Node.Id.of_string id)
          |> Deferred.map ~f:Or_error.join
        in
        printf "done\n";
        return ()
    ]

let add_connection_command =
  let open Command.Let_syntax in
  Command.async_or_error ~summary:"add a connection"
    [%map_open
      let id1 = anon ("NODE-ID-1" %: string)
      and port1 = anon ("PORT-1" %: int)
      and id2 = anon ("NODE-ID-2" %: string)
      and port2 = anon ("PORT-2" %: int)
      and perfect = flag "perfect" no_arg ~doc:" byte- and window- perfect connection"
      in
      fun () ->
        let open Deferred.Or_error.Let_syntax in
        let transformations =
          if perfect then
            Connection.Transformations.empty
          else
            Connection.Transformations.create
              ~changed:1
              ~skewed:true
              ~padded:true
              ~rewindowed:(Some 4)
        in
        let%bind state = State.load () in
        let id1 = Node.Id.of_string id1 in
        let id2 = Node.Id.of_string id2 in
        let extra_bits = ref [] in
        Connection.Transformations.init_extra_bits transformations extra_bits;
        let new_connection =
          let id = Connection.Id.create id1 port1 id2 port2 in
          { Connection.
            id;
            extra_bits;
            transformations;
          }
        in
        let%bind conn =
          let port = state.server_port in
          Rpc.Connection.client
            (Tcp.Where_to_connect.of_host_and_port {host="localhost"; port})
          |> Deferred.Or_error.of_exn_result
        in
        let%bind () =
          Rpcs.Add_connection.dispatch conn new_connection
          |> Deferred.map ~f:Or_error.join
        in
        printf "done\n";
        return ()
    ]

let drop_connection_command =
  let open Command.Let_syntax in
  Command.async_or_error ~summary:"drop a connection"
    [%map_open
      let id1 = anon ("NODE-ID-1" %: string)
      and port1 = anon ("PORT-1" %: int)
      and id2 = anon ("NODE-ID-2" %: string)
      and port2 = anon ("PORT-2" %: int)
      in
      fun () ->
        let open Deferred.Or_error.Let_syntax in
        let%bind state = State.load () in
        let id1 = Node.Id.of_string id1 in
        let id2 = Node.Id.of_string id2 in
        let new_connection =
          let id = Connection.Id.create id1 port1 id2 port2 in
          { Connection.
            id;
            extra_bits = ref [];
            transformations = Connection.Transformations.empty;
          }
        in
        let%bind conn =
          let port = state.server_port in
          Rpc.Connection.client
            (Tcp.Where_to_connect.of_host_and_port {host="localhost"; port})
          |> Deferred.Or_error.of_exn_result
        in
        let%bind () =
          Rpcs.Drop_connection.dispatch conn new_connection
          |> Deferred.map ~f:Or_error.join
        in
        printf "done\n";
        return ()
    ]

let print_dot_command =
  let open Command.Let_syntax in
  Command.async_or_error ~summary:"print dot representation of network"
    [%map_open
       let () = return ()
      in
      fun () ->
        let open Deferred.Or_error.Let_syntax in
        let%bind state = State.load () in
        print_endline (State.to_dot_format state);
        return ()
    ]

let command =
  Command.group ~summary:"sdn-local client" [
    ("add-node",        add_node_command        );
    ("drop-node",       drop_node_command       );
    ("add-connection",  add_connection_command  );
    ("drop-connection", drop_connection_command );
    ("print-dot"      , print_dot_command       );
  ]
