open! Core
open! Async

include Rpcs_intf

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

module Make_pipe (X : sig
    type query [@@deriving bin_io]
    type response [@@deriving bin_io]
    val name : string
    val version : int
  end) : S_pipe with type query = X.query with type response = X.response
= struct
  include X
  type error = Error.t [@@deriving bin_io]
  let rpc = Rpc.Pipe_rpc.create ~name ~version ~bin_query ~bin_response ~bin_error ()
  let dispatch = Rpc.Pipe_rpc.dispatch rpc
  let implement f = Rpc.Pipe_rpc.implement rpc f
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

module Register = struct
  include Make_pipe (struct
      type query = Node.Id.t [@@deriving bin_io]
      type response = Message.t [@@deriving bin_io]
      let name = "register"
      let version = 1
    end)
end

module Register_response = struct
  include Make_pipe (struct
      type query = unit [@@deriving bin_io]
      type response = Message.t [@@deriving bin_io]
      let name = "register-response"
      let version = 1
    end)
end
