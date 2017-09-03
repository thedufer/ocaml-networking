open! Core
open! Async

module type S = sig
  type query
  type response
  val dispatch : Rpc.Connection.t -> query -> response Deferred.Or_error.t
  val implement : ('state -> query -> response Deferred.t) -> 'state Rpc.Implementation.t
end

module Make (X : sig
    type query [@@deriving bin_io]
    type response [@@deriving bin_io]
    val name : string
    val version : int
  end) : S with type query = X.query with type response = X.response
= struct
  include X
  let rpc = Rpc.Rpc.create ~name ~version ~bin_query ~bin_response
  let dispatch = Rpc.Rpc.dispatch rpc
  let implement f = Rpc.Rpc.implement rpc f
end

module Add_node = struct
  include Make (struct
      type query = Node.t [@@deriving bin_io]
      type response = unit Or_error.t [@@deriving bin_io]
      let name = "add-node"
      let version = 1
    end)
end

module Drop_node = struct
  include Make (struct
      type query = Node.Id.t [@@deriving bin_io]
      type response = unit Or_error.t [@@deriving bin_io]
      let name = "drop-node"
      let version = 1
    end)
end

module Add_connection = struct
  include Make (struct
      type query = Connection.t [@@deriving bin_io]
      type response = unit Or_error.t [@@deriving bin_io]
      let name = "add-connection"
      let version = 1
    end)
end

module Drop_connection = struct
  include Make (struct
      type query = Connection.t [@@deriving bin_io]
      type response = unit Or_error.t [@@deriving bin_io]
      let name = "drop-connection"
      let version = 1
    end)
end
