open! Core

module Transformations = struct
  type t = {
    changed : int;
    skewed : bool;
    padded : bool;
    rewindowed : int option;
  } [@@deriving bin_io, compare, fields, sexp]

  let empty = {
    changed    = 0;
    skewed     = false;
    padded     = false;
    rewindowed = None;
  }

  let create = Fields.create

  let param =
    let open Command.Let_syntax in
    [%map_open
      let changed =
        flag "changed" (optional_with_default 0 int) ~doc:"N bits flipped per kilobyte"
      and skewed =
        flag "skewed" no_arg ~doc:" skewed bytes"
      and padded =
        flag "padded" no_arg ~doc:" padding between messages"
      and rewindowed =
        flag "rewindowed" (optional int) ~doc:"N bytes per message"
      in
      {changed; skewed; padded; rewindowed}
    ]

  let char_to_bools c =
    let c = Char.to_int c in
    List.init 8 ~f:(fun i ->
      ((1 lsl i) land c) > 0)

  let bools_to_char bs =
    List.foldi bs ~init:0 ~f:(fun i c b ->
        (Bool.to_int b lsl i) lor c)
    |> Char.of_int_exn

  let chunks l n =
    assert (n > 0);
    let (rev_bytes, rev_remaining_bits) =
      List.fold l ~init:([], [])
        ~f:(fun (rev_chunks, rev_remaining_elts) elt ->
            if List.length rev_remaining_elts = n then
              let new_byte = List.rev (elt :: rev_remaining_elts) in
              (new_byte :: rev_chunks, [])
            else
              (rev_chunks, elt :: rev_remaining_elts))
    in
    (List.rev rev_bytes, List.rev rev_remaining_bits)

  let bools_to_bytes bs =
    let (chunks, remaining_bits) = chunks bs 8 in
    (List.map chunks ~f:bools_to_char, remaining_bits)

  (* Up to [0...n] random bits. *)
  let random_bits n =
    List.init (Random.int n) ~f:(fun _ -> Random.bool ())

  let skew_start extra_bits =
    extra_bits := random_bits 8

  let change_data data n =
    List.map data ~f:(fun b ->
        if Random.int (8 * 1000) < n
        then not b
        else b)

  let init_extra_bits t extra_bits =
    if t.skewed
    then skew_start extra_bits
    else ()

  let map_data t extra_bits old_data =
    let old_bits =
      !extra_bits @
      List.concat_map old_data ~f:char_to_bools
    in
    let changed = change_data old_bits t.changed in
    let padded =
      if t.padded then
        let len = List.length changed in
        random_bits len @
        changed @
        random_bits len
      else
        changed
    in
    let (rewindowed, new_extra_bits) =
      let (bytes, extra_bits) = bools_to_bytes padded in
      match t.rewindowed with
      | None -> ([bytes], extra_bits)
      | Some n ->
        let (chunks, extra_bytes) = chunks bytes n in
        (chunks, List.concat_map extra_bytes ~f:char_to_bools @ extra_bits)
    in
    extra_bits := new_extra_bits;
    rewindowed
end

type t = {
  node1 : Node.Id.t;
  port1 : int;
  node2 : Node.Id.t;
  port2 : int;
  transformations : Transformations.t;
  extra_bits : bool list ref;
} [@@deriving bin_io, compare, sexp]

let uses_port t (id, port) =
  (Node.Id.equal id t.node1 &&
   Int.equal port t.port1
  ) ||
  (Node.Id.equal id t.node2 &&
   Int.equal port t.port2)

let equal = [%compare.equal: t]

let same_ports t1 t2 = uses_port t1 (t2.node1, t2.port1) && uses_port t1 (t2.node2, t2.port2)

let get_connected_port_and_connection ts (id, port) =
  List.find ts ~f:(fun t -> uses_port t (id, port))
  |> Option.map ~f:(fun connection ->
      if Node.Id.equal connection.node1 id then
        (connection.node2, connection.port2, connection)
      else
        (connection.node1, connection.port1, connection))
