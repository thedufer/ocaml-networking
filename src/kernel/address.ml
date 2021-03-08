open Core_kernel

include Unique_id.Int63 ()

(* We number from 1, so it is very unlikely we'll get past the bounds of a
   31-bit int. *)
let of_int = of_int_exn
let to_int = to_int_exn

let arg_type = Command.Arg_type.map (Command.Param.int) ~f:of_int
