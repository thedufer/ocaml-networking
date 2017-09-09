open! Core

module Type = struct
  type t =
    | Perfect
    | Changed of int
  [@@deriving bin_io, compare, sexp]
end

type t = {
  node1 : Node.Id.t;
  port1 : int;
  node2 : Node.Id.t;
  port2 : int;
  type_ : Type.t;
} [@@deriving bin_io, compare, sexp]

let uses_port t (id, port) =
  (Node.Id.equal id t.node1 &&
   Int.equal port t.port1
  ) ||
  (Node.Id.equal id t.node2 &&
   Int.equal port t.port2)

let equal = [%compare.equal: t]

let same_ports t1 t2 = uses_port t1 (t2.node1, t2.port1) && uses_port t1 (t2.node2, t2.port2)

let get_connected_port ts (id, port) =
  List.find ts ~f:(fun t -> uses_port t (id, port))
  |> Option.map ~f:(fun connection ->
      if Node.Id.equal connection.node1 id then
        (connection.node2, connection.port2)
      else
        (connection.node1, connection.port1))
