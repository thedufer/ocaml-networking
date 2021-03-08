open Core_kernel

type t [@@deriving bin_io, compare, sexp]

include Comparable with type t := t
include Stringable with type t := t

val create : unit -> t

val of_int : int -> t
val to_int : t -> int

val arg_type : t Command.Arg_type.t
