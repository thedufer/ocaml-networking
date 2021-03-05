open Core_kernel

module Transformations : sig
  type t

  val empty : t

  val create
    :  changed:int
    -> skewed:bool
    -> padded:bool
    -> rewindowed:int option
    -> t

  val map_data : t -> bool list ref -> char list -> char list list

  val init_extra_bits : t -> bool list ref -> unit

  val param : t Command.Param.t
end

type t = {
  node1 : Node.Id.t;
  port1 : int;
  node2 : Node.Id.t;
  port2 : int;
  transformations : Transformations.t;
  extra_bits : bool list ref;
} [@@deriving bin_io, sexp]

val equal : t -> t -> bool

val same_ports : t -> t -> bool

val uses_port : t -> Node.Id.t * int -> bool

val get_connected_port_and_connection
  :  t list
  -> Node.Id.t * int
  -> (Node.Id.t * int * t) option
