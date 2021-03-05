open Core_kernel

(* A message arriving on a hardware port numbered [port], containing [data]. The
first bit is the low-order in position 0 in the list. *)

type t = {
  port : int;
  data : char list;
} [@@deriving bin_io, sexp]
