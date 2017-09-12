open! Core

module Id : sig
  type t [@@deriving bin_io, compare, sexp]

  include Stringable with type t := t

  val equal : t -> t -> bool
end

type t = {
  id : Id.t;
  ports : int;
} [@@deriving bin_io, sexp]

val has_port : t -> int -> bool
