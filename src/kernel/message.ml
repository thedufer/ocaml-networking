open Core_kernel

type t = {
  port : int;
  data : char list;
} [@@deriving bin_io, sexp]
