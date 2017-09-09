open Core

module Type : sig
  type t =
    | Perfect
    (* [int] is number of bits changed per kilobyte *)
    | Changed of int
    | Skewed of bool list ref
  [@@deriving bin_io, sexp]

  val param : t Command.Param.t

  val map_data : t -> char list -> char list
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

val get_connected_port_and_type
  :  t list
  -> Node.Id.t * int
  -> (Node.Id.t * int * Type.t) option
