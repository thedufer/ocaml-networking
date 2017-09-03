open Core
open Async

module type S = sig
  type query
  type response
  val dispatch : Rpc.Connection.t -> query -> response Deferred.Or_error.t
  val implement : ('state -> query -> response Deferred.t) -> 'state Rpc.Implementation.t
end

module Add_node : S
  with type query = Node.t
   and type response = unit Or_error.t

module Drop_node : S
  with type query = Node.Id.t
   and type response = unit Or_error.t

module Add_connection : S
  with type query = Connection.t
   and type response = unit Or_error.t

module Drop_connection : S
  with type query = Connection.t
   and type response = unit Or_error.t
