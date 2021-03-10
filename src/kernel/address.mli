open Core_kernel

module Node : sig
  type t [@@deriving bin_io, compare, sexp]

  include Stringable with type t := t

  val create : unit -> t
  val record : t -> unit
end

type t [@@deriving bin_io, compare, sexp]

include Comparable with type t := t
include Stringable with type t := t

val create : Node.t -> int -> t

val to_int64 : t -> Int64.t
val of_int64 : Int64.t -> t

val arg_type : t Command.Arg_type.t
