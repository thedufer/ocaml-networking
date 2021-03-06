open Core
open Async
open Sdn_local_protocol_kernel
open Deferred.Or_error.Let_syntax

let implementations pipe =
  let implementations =
    Rpcs.Register_response.implement (fun () () ->
      return pipe)
    :: []
  in
  Rpc.Implementations.create_exn
    ~implementations
    ~on_unknown_rpc:`Close_connection

let connect id =
  let%bind state = State.load () in
  let (r_pipe, w_pipe) = Pipe.create () in
  let implementations =
    { Rpc.Connection.Client_implementations.
      connection_state = Fn.const ();
      implementations = implementations r_pipe;
    }
  in
  let%bind conn =
    let port = state.server_port in
    Rpc.Connection.client
      (Tcp.Where_to_connect.of_host_and_port {host="localhost"; port})
      ~implementations
    |> Deferred.Or_error.of_exn_result
  in
  let%bind (r_pipe, _) =
    Rpcs.Register.dispatch conn id
    |> Deferred.map ~f:Or_error.join
  in
  let%bind (address, ports) =
    let%bind state = State.load () in
    let node =
      List.find_exn state.nodes ~f:(fun node ->
          Node.Id.equal node.id id)
    in
    return (node.address, node.ports)
  in
  return (r_pipe, w_pipe, address, ports)
