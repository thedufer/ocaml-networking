open Core

type t [@@deriving bin_io, compare, sexp]

include Comparable with type t := t

val create : unit -> t
