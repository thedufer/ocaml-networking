open Core_kernel

module Node = struct
  type t = int [@@deriving bin_io, compare, sexp]

  let to_string = Int.to_string

  let of_string = Int.of_string

  (* TODO what about when it restarts and reads existing addresses? *)
  let create =
    let prev = ref (-1) in
    fun () -> incr prev; !prev
end

module T = struct
  type t =
    { node : Node.t
    ; port : int
    } [@@deriving bin_io, compare]

  let create node port = { node; port }
                         
  let of_string s =
    let (node, port) = String.lsplit2_exn s ~on:':' in
    { node = Node.of_string node; port = Int.of_string port }

  let to_string { node; port } =
    sprintf !"%{Node}:%d" node port
end

module T2 = struct
  include T
  include Sexpable.Of_stringable (T)
end

include T2
include Comparable.Make (T2)


let arg_type = Command.Arg_type.map Command.Param.string ~f:of_string
