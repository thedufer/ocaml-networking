open! Core_kernel

type t = {
  from : Node.Id.t * int;
  to_  : Node.Id.t * int;
  sent : char list;
  received : char list list;
  delivered : bool;
} [@@deriving sexp]
