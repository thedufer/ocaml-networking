open! Core

type t = {
  node1 : Node.Id.t;
  port1 : int;
  node2 : Node.Id.t;
  port2 : int;
} [@@deriving bin_io, sexp]

let uses_port t (id, port) =
  (Node.Id.equal id t.node1 &&
   Int.equal port t.port1
  ) ||
  (Node.Id.equal id t.node2 &&
   Int.equal port t.port2)

let equal t1 t2 = uses_port t1 (t2.node1, t2.port1) && uses_port t1 (t2.node2, t2.port2)
