open! Core

type t = char list

let of_int64 int ~num_bytes =
  assert (num_bytes <= 8);
  assert Big_int.(lt_big_int (big_int_of_int64 int) (power_int_positive_int 2 (num_bytes * 8)));
  List.init num_bytes ~f:(fun i ->
      let shift = i * 8 in
      Int64.((int lsr shift) land 0xffL |> to_int_exn |> Char.of_int_exn))
  |> List.rev

let to_int64 bytes ~num_bytes =
  assert (List.length bytes = num_bytes);
  List.rev bytes
  |> List.foldi ~init:0L ~f:(fun i acc byte ->
      let shift = i * 8 in
      Int64.(((Char.to_int byte |> Int64.of_int) lsl shift) lor acc))

let of_int int ~num_bytes = of_int64 (Int64.of_int int) ~num_bytes
let to_int bytes ~num_bytes = to_int64 bytes ~num_bytes |> Int64.to_int_exn
