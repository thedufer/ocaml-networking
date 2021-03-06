open! Core_kernel

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

  let create ~changed ~skewed ~padded ~rewindowed =
    begin
      match rewindowed with
      | None -> ()
      | Some x -> assert (x > 0)
    end;
    Fields.create ~changed ~skewed ~padded ~rewindowed

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

  let bools_to_bytes bs =
    let (chunks, remaining_bits) = Util.chunks bs 8 in
    (List.map chunks ~f:Util.bools_to_char, remaining_bits)

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
      List.concat_map old_data ~f:Util.char_to_bools
    in
    let changed = change_data old_bits t.changed in
    let padded =
      if t.padded then
        let len = List.length changed / 2 in
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
        let (chunks, extra_bytes) = Util.chunks bytes n in
        (chunks, List.concat_map extra_bytes ~f:Util.char_to_bools @ extra_bits)
    in
    extra_bits := new_extra_bits;
    rewindowed
end

module Id = struct
  type t = {
    node1 : Node.Id.t;
    port1 : int;
    node2 : Node.Id.t;
    port2 : int;
  } [@@deriving bin_io, compare, sexp]

  let create nodea porta nodeb portb =
    if [%compare: Node.Id.t * int] (nodea, porta) (nodeb, portb) >= 0 then
      {node1 = nodea; port1 = porta; node2 = nodeb; port2 = portb}
    else
      {node1 = nodeb; port1 = portb; node2 = nodea; port2 = porta}

  let uses_port t (id, port) =
    (Node.Id.equal id t.node1 &&
     Int.equal port t.port1
    ) ||
    (Node.Id.equal id t.node2 &&
     Int.equal port t.port2)

  let to_html_id {node1; port1; node2; port2} =
    sprintf !"%{Node.Id}-%d-%{Node.Id}-%d" node1 port1 node2 port2
end

type t = {
  id : Id.t;
  transformations : Transformations.t;
  extra_bits : bool list ref;
} [@@deriving bin_io, compare, sexp]

let uses_port t (id, port) = Id.uses_port t.id (id, port)

let equal = [%compare.equal: t]

let get_connected_port_and_connection ts (id, port) =
  List.find ts ~f:(fun t -> uses_port t (id, port))
  |> Option.map ~f:(fun connection ->
      if Node.Id.equal connection.id.node1 id then
        (connection.id.node2, connection.id.port2, connection)
      else
        (connection.id.node1, connection.id.port1, connection))
