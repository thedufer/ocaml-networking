open! Core
open! Async
open Sdn_local_protocol_kernel

type t = {
  server_port : int;
  nodes : Node.t list;
  connections : Connection.t list;
} [@@deriving sexp]

let init ~server_port = {server_port; nodes = []; connections = []}

let location () = Unix.getenv_exn "HOME" ^/ "sdn-local.state"

let save t =
  let sexp = sexp_of_t t in
  Writer.save_sexp (location ()) sexp
  |> Deferred.ok

let load () = Reader.load_sexp (location ()) t_of_sexp

open Or_error.Let_syntax

let has_node_with_id t id =
  List.exists t.nodes ~f:(fun node -> Node.Id.equal id node.id)

let add_node t (id : Node.Id.t) ports =
  if has_node_with_id t id then
    Or_error.error_string "node with id already exists"
  else
    let nodes =
      {Node. id; ports; address = Address.Node.create () } :: t.nodes
    in
    Ok {t with nodes}

let drop_node t (id : Node.Id.t) =
  if has_node_with_id t id then
    let nodes =
      List.filter t.nodes ~f:(fun node ->
        not (Node.Id.equal node.id id))
    in
    let connections =
      List.filter t.connections ~f:(fun connection ->
          not (Node.Id.equal connection.id.node1 id ||
               Node.Id.equal connection.id.node2 id))
    in
    Ok {t with nodes; connections}
  else
    Or_error.error_string "node doesn't exist"

let add_connection t (connection : Connection.t) =
  let%bind () =
    if Node.Id.equal connection.id.node1 connection.id.node2 &&
       Int.equal connection.id.port1 connection.id.port2 then
      Or_error.error_s [%message "can't connect port to itself"]
    else
      Ok ()
  in
  let%bind () =
    let test_end id port =
      let%bind () =
        match List.find t.nodes ~f:(fun node -> Node.Id.equal node.id id) with
        | None -> Or_error.error_s [%message "node not found" (id : Node.Id.t)]
        | Some node ->
          if Node.has_port node port then
            Ok ()
          else
            Or_error.error_s [%message "node does not have port"
                (id : Node.Id.t)
                (port : int)]
      in
      if List.exists t.connections ~f:(fun c ->
          Connection.uses_port c (id, port)) then
        Or_error.error_s [%message "port in use"
            (id : Node.Id.t)
            (port : int)]
      else
        Ok ()
    in
    let%bind () = test_end connection.id.node1 connection.id.port1 in
    test_end connection.id.node2 connection.id.port2
  in
  let connections = connection :: t.connections in
  Ok {t with connections}

let drop_connection t connection =
  let conn_equal_id =
    Comparable.lift ~f:(fun conn -> conn.Connection.id) [%compare.equal: Connection.Id.t]
  in
  if List.mem ~equal:conn_equal_id t.connections connection then
    let connections =
      List.filter t.connections ~f:(fun connection' ->
          not (conn_equal_id connection connection'))
    in
    Ok {t with connections}
  else
    Or_error.error_string "connection already exists"

let to_dot_format t =
  let subgraphs =
    List.concat_map t.nodes ~f:(fun node ->
        let port_nodes =
          List.init node.ports ~f:(fun port ->
              sprintf !"    %{Node.Id}%d [label=\"%d\" id=\"%{Node.Id}-%d\"];" node.id port port node.id port)
        in
        [sprintf !"  subgraph cluster_%{Node.Id} {" node.id] @
        [sprintf !"    id=%{Node.Id};" node.id] @
        [         "    style=filled;"] @
        [         "    color=lightgrey;"] @
        [sprintf !"    label = \"%{Node.Id} (%{Address.Node})\"" node.id node.address] @
        port_nodes @
        [         "  }"]
      )
  in
  let edges =
    List.map t.connections
      ~f:(fun {id = {node1; port1; node2; port2} as id; transformations = _; extra_bits = _} ->
          sprintf !"  %{Node.Id}%d -- %{Node.Id}%d [id=\"%s\"];" node1 port1 node2 port2 (Connection.Id.to_html_id id))
  in
  String.concat ~sep:"\n" @@
  ["graph network {"] @
  subgraphs @
  edges @
  ["}"]
