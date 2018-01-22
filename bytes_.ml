open! Core

type t = char list

let of_int int ~num_bytes =
  assert (num_bytes <= 8);
  assert Big_int.(lt_big_int (big_int_of_int int) (power_int_positive_int 2 (num_bytes * 8)));
  List.init num_bytes ~f:(fun i ->
      (int lsr (i * 8)) land 0xff |> Char.of_int_exn
    )
  |> List.rev

let to_int bytes ~num_bytes =
  assert (List.length bytes = num_bytes);
  List.rev bytes
  |> List.foldi ~init:0 ~f:(fun i acc byte ->
      ((Char.to_int byte) lsl (i * 8)) lor acc)
