open Core
open Async
open Sdn_local_protocol_kernel

type t = {
  server_port : int;
  nodes : Node.t list;
  connections : Connection.t list;
} [@@deriving sexp]

val init : server_port:int -> t

val save : t -> unit Deferred.Or_error.t
val load : unit -> t Deferred.Or_error.t

val add_node : t -> Node.Id.t -> int -> t Or_error.t
val drop_node : t -> Node.Id.t -> t Or_error.t

val add_connection : t -> Connection.t -> t Or_error.t
val drop_connection : t -> Connection.t -> t Or_error.t

val to_dot_format : t -> string
