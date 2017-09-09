open Core

module Type : sig
  type t =
    | Perfect
    (* [int] is number of bits changed per kilobyte *)
    | Changed of int
  [@@deriving bin_io, sexp]
end

type t = {
  node1 : Node.Id.t;
  port1 : int;
  node2 : Node.Id.t;
  port2 : int;
  type_ : Type.t;
} [@@deriving bin_io, sexp]

val equal : t -> t -> bool

val same_ports : t -> t -> bool

val uses_port : t -> Node.Id.t * int -> bool

val get_connected_port : t list -> Node.Id.t * int -> (Node.Id.t * int) option
