open Core

type t = {
  port : int;
  data : char list;
} [@@deriving bin_io, sexp]
