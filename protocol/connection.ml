open! Core

module Type = struct
  type t =
    | Perfect
    | Changed of int
    | Skewed of bool list ref
  [@@deriving bin_io, compare, sexp]

  let skew_start () =
    List.init (Random.int 8) ~f:(fun _ -> Random.bool ())
    |> ref

  let param =
    let open Command.Param in
    choose_one ~if_nothing_chosen:(`Default_to Perfect) [
      (flag "changed" (optional int) ~doc:"N bits flipped per kilobyte"
       |> map ~f:(Option.map ~f:(fun n -> Changed n)));
      (flag "skewed" no_arg ~doc:" skewed bytes"
       |> map ~f:(fun b -> Option.some_if b (Skewed (skew_start ()))));
    ]

  let char_to_bools c =
    let c = Char.to_int c in
    List.init 8 ~f:(fun i ->
      ((1 lsl i) land c) > 0)

  let bools_to_char bs =
    List.foldi bs ~init:0 ~f:(fun i c b ->
        (Bool.to_int b lsl i) lor c)
    |> Char.of_int_exn

  let map_data t data =
    match t with
    | Perfect -> data
    | Changed n ->
      List.map data ~f:(fun c ->
          char_to_bools c
          |> List.map ~f:(fun b ->
              if Random.int (8 * 1000) < n
              then not b
              else b)
          |> bools_to_char)
    | Skewed old_bits ->
      List.map data ~f:(fun c ->
          let bits = !old_bits @ char_to_bools c in
          let (bits_to_pass, bits_to_store) = List.split_n bits 8 in
          old_bits := bits_to_store;
          bools_to_char bits_to_pass)
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

let get_connected_port_and_type ts (id, port) =
  List.find ts ~f:(fun t -> uses_port t (id, port))
  |> Option.map ~f:(fun connection ->
      if Node.Id.equal connection.node1 id then
        (connection.node2, connection.port2, connection.type_)
      else
        (connection.node1, connection.port1, connection.type_))
