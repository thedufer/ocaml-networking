open Core

type t = {
  node1 : Node.Id.t;
  port1 : int;
  node2 : Node.Id.t;
  port2 : int;
} [@@deriving bin_io, sexp]

val equal : t -> t -> bool

val uses_port : t -> Node.Id.t * int -> bool
