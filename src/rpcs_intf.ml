open Core
open Async

module type S = sig
  type query
  type response
  val dispatch : Rpc.Connection.t -> query -> response Deferred.Or_error.t
  val implement : ('state -> query -> response Deferred.t) -> 'state Rpc.Implementation.t
end

module type S_pipe = sig
  type query
  type response
  val dispatch
    :  Rpc.Connection.t
    -> query
    -> (response Pipe.Reader.t * Rpc.Pipe_rpc.Metadata.t) Or_error.t Deferred.Or_error.t
  val implement
    : ('state
       -> query
       -> response Async_kernel.Pipe.Reader.t Deferred.Or_error.t)
    -> 'state Rpc.Implementation.t
end

module type Rpcs = sig
  module type S = S

  module Add_node : S
    with type query = Node.Id.t * int
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

  module Register : S_pipe
    with type query = Node.Id.t
     and type response = Message.t

  module Register_response : S_pipe
    with type query = unit
     and type response = Message.t
end
